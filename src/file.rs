use std::{
    fmt::Display,
    num::{NonZeroU32, NonZeroU64},
    sync::Arc,
};

use bytes::{Buf, BufMut, Bytes, BytesMut};
use chrono::{Datelike, NaiveDate, NaiveDateTime, Timelike};
use miniz_oxide::{deflate::compress_to_vec_zlib, inflate::decompress_to_vec_zlib_with_limit};

use crate::{
    calculate_hmac,
    content::{Content, ContentType, DataType},
    Error, Result,
};

#[non_exhaustive]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub enum Version {
    #[default]
    V1,
}

impl Display for Version {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Version::V1 => "SmileBASIC 3/BIG (3DS, Wii U)",
        })
    }
}

impl TryFrom<u16> for Version {
    type Error = Error;

    fn try_from(value: u16) -> Result<Self> {
        match value {
            (0..=3) => Ok(Version::V1),

            _ => Err(Error::UnrecognizedVersion(value)),
        }
    }
}

impl From<Version> for u16 {
    fn from(val: Version) -> Self {
        match val {
            Version::V1 => 1,
        }
    }
}

impl Version {
    pub(crate) const fn header_size(self) -> usize {
        match self {
            Version::V1 => 0x50,
        }
    }

    pub(crate) const fn header_padding(self) -> usize {
        match self {
            Version::V1 => 0,
        }
    }

    pub(crate) const fn author_name_len(self) -> usize {
        match self {
            Version::V1 => 18,
        }
    }
    pub(crate) const fn data_version(self) -> &'static [u8; 4] {
        match self {
            Version::V1 => b"0001",
        }
    }
    pub(crate) const fn file_name_len(self) -> usize {
        match self {
            Version::V1 => 16,
        }
    }

    pub(crate) fn resolve_file_type(&self, header_type: u16) -> FileType {
        match (self, header_type) {
            (_, 0) => FileType::Text,
            (_, 1) => FileType::Data,
            (_, 2) => FileType::Project,
            (_, other) => FileType::Unknown(other),
        }
    }

    pub(crate) fn try_resolve_data_type(&self, data_type: u16) -> Result<DataType> {
        match (self, data_type) {
            (_, 0) => Ok(DataType::I8),
            (_, 1) => Ok(DataType::U8),
            (_, 2) => Ok(DataType::I16),
            (_, 3) => Ok(DataType::U16),
            (_, 4) => Ok(DataType::I32),
            (_, 5) => Ok(DataType::F64),
            (_, _) => Err(Error::UnrecognizedDataType(*self, data_type)),
        }
    }

    pub(crate) fn data_type_to_u16(&self, data_type: DataType) -> Result<u16> {
        match (self, data_type) {
            (_, DataType::I8) => Ok(0),
            (_, DataType::U8) => Ok(1),
            (_, DataType::I16) => Ok(2),
            (_, DataType::U16) => Ok(3),
            (_, DataType::I32) => Ok(4),
            (_, DataType::F64) => Ok(5),
        }
    }

    pub(crate) fn file_type_to_u16(&self, file_type: FileType) -> Result<u16> {
        match (self, file_type) {
            (_, FileType::Unknown(val)) => Ok(val),

            (_, FileType::Text) => Ok(0),
            (_, FileType::Data) => Ok(1),
            (Version::V1, FileType::Project) => Ok(2),
        }
    }
}

#[non_exhaustive]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum FileType {
    Text,
    Data,
    Project,
    Unknown(u16),
}

impl FileType {
    pub fn supported_by(&self, version: Version) -> bool {
        match (version, self) {
            (_, FileType::Unknown(_)) => true,
            (_, FileType::Text) => true,
            (_, FileType::Data) => true,
            (_, FileType::Project) => true,
        }
    }

    pub fn minimum_version(&self) -> Version {
        match self {
            FileType::Text | FileType::Data | FileType::Project | FileType::Unknown(_) => {
                Version::V1
            }
        }
    }

    pub(crate) const fn has_footer(self) -> bool {
        match self {
            FileType::Unknown(_) => false,

            FileType::Text | FileType::Data | FileType::Project => true,
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct Header {
    pub icon: u16,
    pub modify_date: NaiveDateTime,
    pub compressed: bool,
    pub protected: bool,
    pub content_size: u32,
    pub creator: Option<Author>,
    pub editor: Option<Author>,
    pub creator_upload_id: Option<NonZeroU64>,
    pub editor_upload_id: Option<NonZeroU64>,
}

#[derive(Clone, Debug)]
pub struct Author {
    pub id: NonZeroU32,
    pub name: Arc<str>,
}

#[derive(Clone, Debug)]
pub struct File {
    pub(crate) header: Header,
    pub(crate) content: Content,
}

impl Default for File {
    fn default() -> Self {
        Self {
            header: Default::default(),
            content: Content::Parsed(Version::V1, ContentType::Text(String::new())),
        }
    }
}

impl File {
    const FOOTER_SIZE: usize = 20;
    const MAX_RAW_SIZE: usize = 16 * 1024 * 1024; // 16MiB

    pub fn new() -> Self {
        Self::default()
    }

    pub fn into_parsed(mut self) -> Result<Self> {
        self.content = self.content.into_parsed(Some(&self.header))?;
        Ok(self)
    }

    pub fn parse<T: Into<Bytes>>(bytes: T) -> Result<Self> {
        Self::parse_with_inherit(&mut bytes.into(), None)
    }

    pub(crate) fn parse_with_inherit(
        reader: &mut Bytes,
        inherit_from: Option<&Header>,
    ) -> Result<Self> {
        need!(reader, 4);
        let version = Version::try_from(reader.get_u16_le())?;
        need!(reader, version.header_size() - 2);

        let file_type = version.resolve_file_type(reader.get_u16_le());
        let (compressed, protected) = {
            let flags = reader.get_u16_le();
            (flags & 1 != 0, flags & 2 != 0)
        };

        let icon = reader.get_u16_le();
        let content_size = reader.get_u32_le();
        let modify_date = NaiveDate::from_ymd_opt(
            reader.get_u16_le().into(),
            reader.get_u8().into(),
            reader.get_u8().into(),
        )
        .and_then(|date| {
            date.and_hms_opt(
                reader.get_u8().into(),
                reader.get_u8().into(),
                reader.get_u8().into(),
            )
        })
        .ok_or(Error::InvalidHeaderModifyDate)?;

        let _modify_dow = reader.get_u8();
        let author_name_len = version.author_name_len();

        let (mut creator_name, mut editor_name) = (
            reader.copy_to_bytes(author_name_len),
            reader.copy_to_bytes(author_name_len),
        );

        let (creator_uid, editor_uid) = (
            NonZeroU32::new(reader.get_u32_le()),
            NonZeroU32::new(reader.get_u32_le()),
        );

        let creator = creator_uid
            .map(move |id| Author {
                id,
                name: {
                    creator_name.truncate(
                        creator_name
                            .iter()
                            .position(|&b| b == b'\0')
                            .unwrap_or(author_name_len),
                    );
                    String::from_utf8_lossy(&creator_name).into()
                },
            })
            .or(inherit_from.and_then(|header| header.creator.clone()));

        let editor = editor_uid
            .map(move |id| Author {
                id,
                name: {
                    editor_name.truncate(
                        editor_name
                            .iter()
                            .position(|&b| b == b'\0')
                            .unwrap_or(author_name_len),
                    );
                    String::from_utf8_lossy(&editor_name).into()
                },
            })
            .or(inherit_from.and_then(|header| header.editor.clone()));

        let (creator_upload_id, editor_upload_id) = (
            NonZeroU64::new(reader.get_u64_le())
                .or(inherit_from.and_then(|header| header.creator_upload_id)),
            NonZeroU64::new(reader.get_u64_le())
                .or(inherit_from.and_then(|header| header.editor_upload_id)),
        );

        let (content, footer) = if file_type.has_footer() {
            (
                reader.split_to(reader.remaining() - Self::FOOTER_SIZE),
                Some(reader.clone()),
            )
        } else {
            (reader.clone(), None)
        };

        // TODO: Footer check

        let content = if compressed {
            decompress_to_vec_zlib_with_limit(&content, Self::MAX_RAW_SIZE)?.into()
        } else {
            content
        };

        Ok(Self {
            header: Header {
                icon,
                modify_date,
                compressed,
                protected,
                content_size,
                creator,
                editor,
                creator_upload_id,
                editor_upload_id,
            },
            content: Content::Unparsed(version, file_type, content),
        })
    }

    pub fn to_bytes(&self) -> Result<Bytes> {
        self.to_bytes_inherit(None)
    }

    pub(crate) fn to_bytes_inherit(&self, inherit_from: Option<&Header>) -> Result<Bytes> {
        let mut content = self.content.to_bytes(Some(&self.header))?;
        let content_size = content.len();

        if self.header.compressed {
            content = compress_to_vec_zlib(&content, 5).into();
        }

        let version = self.content.version();
        let mut header = BytesMut::with_capacity(version.header_size());

        header.put_u16_le(version.into());
        header.put_u16_le(version.file_type_to_u16(self.content.file_type())?);

        header.put_u16_le(self.header.compressed as u16 | (self.header.protected as u16) << 1);
        header.put_u16_le(self.header.icon);
        header.put_u32_le(content_size as u32);

        let modify_date = &self.header.modify_date;
        header.put_u16_le(modify_date.year() as u16);
        header.put_u8(modify_date.month() as u8);
        header.put_u8(modify_date.day() as u8);
        header.put_u8(modify_date.hour() as u8);
        header.put_u8(modify_date.minute() as u8);
        header.put_u8(modify_date.second() as u8);
        header.put_u8(modify_date.weekday().num_days_from_sunday() as u8);

        let (creator, editor) = (
            match (
                &self.header.creator,
                inherit_from.and_then(|header| header.creator.as_ref()),
            ) {
                (None, _) => None,
                (Some(author), None) => Some(author),
                (Some(author), Some(inherit_author)) => {
                    if author.id == inherit_author.id {
                        None
                    } else {
                        Some(author)
                    }
                }
            },
            match (
                &self.header.editor,
                inherit_from.and_then(|header| header.editor.as_ref()),
            ) {
                (None, _) => None,
                (Some(author), None) => Some(author),
                (Some(author), Some(inherit_author)) => {
                    if author.id == inherit_author.id {
                        None
                    } else {
                        Some(author)
                    }
                }
            },
        );

        match creator {
            Some(author) => {
                header.put_u32_le(author.id.get());
                header.put(author.name.as_bytes());
                header.put_bytes(0, version.author_name_len() - author.name.len());
            }
            None => header.put_bytes(0, 4 + version.author_name_len()),
        }

        match editor {
            Some(author) => {
                header.put_u32_le(author.id.get());
                header.put(author.name.as_bytes());
                header.put_bytes(0, version.author_name_len() - author.name.len());
            }
            None => header.put_bytes(0, 4 + version.author_name_len()),
        }

        let (creator_upload_id, editor_upload_id) = (
            match (
                &self.header.creator_upload_id,
                inherit_from.and_then(|header| header.creator_upload_id.as_ref()),
            ) {
                (None, _) => 0,
                (Some(id), None) => id.get(),
                (Some(id), Some(inherit_id)) => {
                    if id == inherit_id {
                        0
                    } else {
                        id.get()
                    }
                }
            },
            match (
                &self.header.editor_upload_id,
                inherit_from.and_then(|header| header.editor_upload_id.as_ref()),
            ) {
                (None, _) => 0,
                (Some(id), None) => id.get(),
                (Some(id), Some(inherit_id)) => {
                    if id == inherit_id {
                        0
                    } else {
                        id.get()
                    }
                }
            },
        );

        header.put_u64_le(creator_upload_id);
        header.put_u64_le(editor_upload_id);

        header.put_bytes(0, version.header_padding());

        let mut result = BytesMut::with_capacity(header.len() + content.len());
        result.put(header);
        result.put(content);

        let footer = if !self.content.file_type().has_footer() {
            Bytes::new()
        } else if inherit_from.is_some() {
            Bytes::from_static(&[0u8; 20])
        } else {
            calculate_hmac(&result)
        };

        dbg!(&footer);
        result.put(footer);

        Ok(result.freeze())
    }

    pub fn header(&self) -> &Header {
        &self.header
    }

    pub fn header_mut(&mut self) -> &mut Header {
        &mut self.header
    }

    pub fn content(&self) -> &Content {
        &self.content
    }
}
