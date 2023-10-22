use std::{
    collections::HashMap,
    fmt::Display,
    num::{NonZeroU32, NonZeroU64},
    sync::Arc,
};

use bytes::{Buf, BufMut, Bytes, BytesMut};
use chrono::{NaiveDate, NaiveDateTime};
use miniz_oxide::inflate::decompress_to_vec_zlib_with_limit;
use ndarray::{iter::Axes, Array, Array0, Array1, Array2, Array3, Array4, IxDyn};

use crate::{Error, Result};

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
    const fn header_size(self) -> usize {
        match self {
            Version::V1 => 0x50,
        }
    }
    const fn author_name_len(self) -> usize {
        match self {
            Version::V1 => 18,
        }
    }
    const fn data_version(self) -> &'static [u8; 4] {
        match self {
            Version::V1 => b"0001",
        }
    }
    const fn file_name_len(self) -> usize {
        match self {
            Version::V1 => 16,
        }
    }

    fn resolve_file_type(&self, header_type: u16) -> FileType {
        match (self, header_type) {
            (_, 0) => FileType::Text,
            (_, 1) => FileType::Data,
            (_, 2) => FileType::Project,
            (_, other) => FileType::Unknown(other),
        }
    }

    fn try_resolve_data_type(&self, data_type: u16) -> Result<DataType> {
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

    fn data_type_to_u16(&self, data_type: DataType) -> Result<u16> {
        match (self, data_type) {
            (_, DataType::I8) => Ok(0),
            (_, DataType::U8) => Ok(1),
            (_, DataType::I16) => Ok(2),
            (_, DataType::U16) => Ok(3),
            (_, DataType::I32) => Ok(4),
            (_, DataType::F64) => Ok(5),
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

    const fn has_footer(self) -> bool {
        match self {
            FileType::Unknown(_) => false,

            FileType::Text | FileType::Data | FileType::Project => true,
        }
    }
}

#[derive(Clone, Debug)]
pub struct File {
    header: Header,
    content: Content,
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

    fn parse_with_inherit(reader: &mut Bytes, inherit_from: Option<&Header>) -> Result<Self> {
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
            decompress_to_vec_zlib_with_limit(&content, 16 * 1024 * 1024)?.into()
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

impl File {
    pub fn try_downgrade_to(self, version: Version) -> Result<Self> {
        todo!()
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
pub enum Content {
    Unparsed(Version, FileType, Bytes),
    Parsed(Version, ContentType),
}

impl Content {
    pub fn version(&self) -> Version {
        match self {
            Content::Unparsed(version, _, _) => *version,
            Content::Parsed(version, _) => *version,
        }
    }

    pub fn file_type(&self) -> FileType {
        match self {
            Content::Unparsed(_, file_type, _) => *file_type,
            Content::Parsed(_, content) => content.file_type(),
        }
    }

    pub fn into_parsed(self, header: Option<&Header>) -> Result<Self> {
        match self {
            Content::Parsed(_, _) => Ok(self),
            Content::Unparsed(version, file_type, raw_content) => Ok(Content::Parsed(
                version,
                ContentType::parse(raw_content, version, file_type, header)?,
            )),
        }
    }
}

pub trait FileContentType: Into<ContentType> {
    fn minimum_version(&self) -> Version;
    fn try_downgrade_to(self, version: Version) -> Result<Self>;

    fn try_upgrade_to(self, _version: Version) -> Result<Self> {
        Ok(self)
    }

    fn supported_by(&self, version: Version) -> bool {
        self.minimum_version() <= version
    }
}

#[derive(Clone, Debug)]
pub enum ContentType {
    Text(String),
    Data(DataContent),
    Project(ProjectContent),
}

impl ContentType {
    fn parse(
        reader: Bytes,
        version: Version,
        file_type: FileType,
        header: Option<&Header>,
    ) -> Result<Self> {
        match file_type {
            FileType::Unknown(file_type) => Err(Error::UnrecognizedFileType(version, file_type)),
            FileType::Text => Ok(Self::Text(String::from_utf8_lossy(&reader).into_owned())),
            FileType::Data => Ok(Self::Data(DataContent::parse(reader, version)?)),
            FileType::Project => Ok(Self::Project(ProjectContent::parse(
                reader, version, header,
            )?)),
        }
    }

    fn to_bytes(&self, version: Version, header: Option<&Header>) -> Result<Bytes> {
        match self {
            ContentType::Text(text) => Ok(Bytes::copy_from_slice(text.as_bytes())),
            ContentType::Data(data) => data.to_bytes(version),
            ContentType::Project(project) => project.to_bytes(version, header),
        }
    }

    pub fn file_type(&self) -> FileType {
        match self {
            ContentType::Text(_) => FileType::Text,
            ContentType::Data(_) => FileType::Data,
            ContentType::Project(_) => FileType::Project,
        }
    }

    pub fn text(&self) -> Option<&str> {
        match self {
            ContentType::Text(text) => Some(text),
            _ => None,
        }
    }

    pub fn text_mut(&mut self) -> Option<&mut String> {
        match self {
            ContentType::Text(text) => Some(text),
            _ => None,
        }
    }

    pub fn data(&self) -> Option<&DataContent> {
        match self {
            ContentType::Data(data) => Some(data),
            _ => None,
        }
    }

    pub fn data_mut(&mut self) -> Option<&mut DataContent> {
        match self {
            ContentType::Data(data) => Some(data),
            _ => None,
        }
    }

    pub fn project(&self) -> Option<&ProjectContent> {
        match self {
            ContentType::Project(project) => Some(project),
            _ => None,
        }
    }

    pub fn project_mut(&mut self) -> Option<&mut ProjectContent> {
        match self {
            ContentType::Project(project) => Some(project),
            _ => None,
        }
    }
}

impl FileContentType for ContentType {
    fn minimum_version(&self) -> Version {
        match self {
            ContentType::Text(text) => text.minimum_version(),
            ContentType::Data(data) => data.minimum_version(),
            ContentType::Project(project) => project.minimum_version(),
        }
    }

    fn try_downgrade_to(self, version: Version) -> Result<Self> {
        match self {
            ContentType::Text(text) => text.try_downgrade_to(version).map(ContentType::from),
            ContentType::Data(data) => data.try_downgrade_to(version).map(ContentType::from),
            ContentType::Project(project) => {
                project.try_downgrade_to(version).map(ContentType::from)
            }
        }
    }
}

impl<T: ToOwned<Owned = String>> From<T> for ContentType {
    fn from(value: T) -> Self {
        ContentType::Text(value.to_owned())
    }
}

impl<T: ToOwned<Owned = String>> FileContentType for T {
    fn minimum_version(&self) -> Version {
        Version::V1
    }

    fn try_downgrade_to(self, _version: Version) -> Result<Self> {
        Ok(self)
    }
}

pub type Data<T> = Array<T, IxDyn>;

#[non_exhaustive]
#[derive(Clone, Debug)]
pub enum DataContent {
    I8(Data<i8>),
    U8(Data<u8>),
    I16(Data<i16>),
    U16(Data<u16>),
    I32(Data<i32>),
    F64(Data<f64>),
}

impl From<DataContent> for ContentType {
    fn from(value: DataContent) -> Self {
        ContentType::Data(value)
    }
}

impl FileContentType for DataContent {
    fn minimum_version(&self) -> Version {
        match self {
            DataContent::I8(_)
            | DataContent::U8(_)
            | DataContent::I16(_)
            | DataContent::U16(_)
            | DataContent::I32(_)
            | DataContent::F64(_) => Version::V1,
        }
    }

    fn try_downgrade_to(self, version: Version) -> Result<Self> {
        if self.minimum_version() == version {
            return Ok(self);
        }

        Err(Error::DataTypeUnsupported(version, self.data_type()))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DataType {
    I8,
    U8,
    I16,
    U16,
    I32,
    F64,
}

impl Display for DataType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            DataType::I8 => "i8",
            DataType::U8 => "u8",
            DataType::I16 => "i16",
            DataType::U16 => "u16",
            DataType::I32 => "i32",
            DataType::F64 => "f64",
        })
    }
}

impl DataType {
    fn read_array<R: Buf>(&self, buf: &mut R, dimensions: &[usize]) -> Result<DataContent> {
        let element_count = dimensions.iter().product();

        match self {
            DataType::I8 => {
                need!(buf, element_count);
                let elements = (0..element_count).map(|_| buf.get_i8()).collect();
                Ok(DataContent::I8(
                    Array::from_shape_vec(dimensions, elements).expect("Creating array"),
                ))
            }
            DataType::U8 => {
                need!(buf, element_count);
                let elements = (0..element_count).map(|_| buf.get_u8()).collect();
                Ok(DataContent::U8(
                    Array::from_shape_vec(dimensions, elements).expect("Creating array"),
                ))
            }
            DataType::I16 => {
                need!(buf, element_count * 2);
                let elements = (0..element_count).map(|_| buf.get_i16_le()).collect();
                Ok(DataContent::I16(
                    Array::from_shape_vec(dimensions, elements).expect("Creating array"),
                ))
            }
            DataType::U16 => {
                need!(buf, element_count * 2);
                let elements = (0..element_count).map(|_| buf.get_u16_le()).collect();
                Ok(DataContent::U16(
                    Array::from_shape_vec(dimensions, elements).expect("Creating array"),
                ))
            }
            DataType::I32 => {
                need!(buf, element_count * 4);
                let elements = (0..element_count).map(|_| buf.get_i32_le()).collect();
                Ok(DataContent::I32(
                    Array::from_shape_vec(dimensions, elements).expect("Creating array"),
                ))
            }
            DataType::F64 => {
                need!(buf, element_count * 8);
                let elements = (0..element_count).map(|_| buf.get_f64_le()).collect();
                Ok(DataContent::F64(
                    Array::from_shape_vec(dimensions, elements).expect("Creating array"),
                ))
            }
        }
    }
}

impl DataContent {
    pub fn i8(&self) -> Option<&Data<i8>> {
        match self {
            DataContent::I8(content) => Some(content),
            _ => None,
        }
    }
    pub fn u8(&self) -> Option<&Data<u8>> {
        match self {
            DataContent::U8(content) => Some(content),
            _ => None,
        }
    }
    pub fn i16(&self) -> Option<&Data<i16>> {
        match self {
            DataContent::I16(content) => Some(content),
            _ => None,
        }
    }
    pub fn u16(&self) -> Option<&Data<u16>> {
        match self {
            DataContent::U16(content) => Some(content),
            _ => None,
        }
    }
    pub fn i32(&self) -> Option<&Data<i32>> {
        match self {
            DataContent::I32(content) => Some(content),
            _ => None,
        }
    }
    pub fn f64(&self) -> Option<&Data<f64>> {
        match self {
            DataContent::F64(content) => Some(content),
            _ => None,
        }
    }

    pub fn i8_mut(&mut self) -> Option<&mut Data<i8>> {
        match self {
            DataContent::I8(content) => Some(content),
            _ => None,
        }
    }
    pub fn u8_mut(&mut self) -> Option<&mut Data<u8>> {
        match self {
            DataContent::U8(content) => Some(content),
            _ => None,
        }
    }
    pub fn i16_mut(&mut self) -> Option<&mut Data<i16>> {
        match self {
            DataContent::I16(content) => Some(content),
            _ => None,
        }
    }
    pub fn u16_mut(&mut self) -> Option<&mut Data<u16>> {
        match self {
            DataContent::U16(content) => Some(content),
            _ => None,
        }
    }
    pub fn i32_mut(&mut self) -> Option<&mut Data<i32>> {
        match self {
            DataContent::I32(content) => Some(content),
            _ => None,
        }
    }
    pub fn f64_mut(&mut self) -> Option<&mut Data<f64>> {
        match self {
            DataContent::F64(content) => Some(content),
            _ => None,
        }
    }

    pub fn data_type(&self) -> DataType {
        match self {
            DataContent::I8(_) => DataType::I8,
            DataContent::U8(_) => DataType::U8,
            DataContent::I16(_) => DataType::I16,
            DataContent::U16(_) => DataType::U16,
            DataContent::I32(_) => DataType::I32,
            DataContent::F64(_) => DataType::F64,
        }
    }

    pub fn parse(mut reader: Bytes, version: Version) -> Result<Self> {
        need!(reader, 0x1C);
        let magic = reader.split_to(4);

        if magic.as_ref() != b"PCBN" {
            return Err(Error::InvalidDataHeaderMagic);
        }

        let data_version = reader.split_to(4);

        if data_version.as_ref() != version.data_version() {
            return Err(Error::MismatchedDataVersion(
                data_version.as_ref().try_into().unwrap(),
                *version.data_version(),
            ));
        }

        let data_type = version.try_resolve_data_type(reader.get_u16_le())?;
        let dim_count = reader.get_u16_le() as usize;
        if dim_count > 4 {
            return Err(Error::TooManyDimensions);
        }

        let dimensions = [
            reader.get_u32_le() as usize,
            reader.get_u32_le() as usize,
            reader.get_u32_le() as usize,
            reader.get_u32_le() as usize,
        ];

        data_type.read_array(&mut reader, &dimensions[..dim_count])
    }

    fn ndim(&self) -> usize {
        match self {
            DataContent::I8(array) => array.ndim(),
            DataContent::U8(array) => array.ndim(),
            DataContent::I16(array) => array.ndim(),
            DataContent::U16(array) => array.ndim(),
            DataContent::I32(array) => array.ndim(),
            DataContent::F64(array) => array.ndim(),
        }
    }

    fn dimensions(&self) -> Axes<'_, IxDyn> {
        match self {
            DataContent::I8(array) => array.axes(),
            DataContent::U8(array) => array.axes(),
            DataContent::I16(array) => array.axes(),
            DataContent::U16(array) => array.axes(),
            DataContent::I32(array) => array.axes(),
            DataContent::F64(array) => array.axes(),
        }
    }

    fn to_bytes(&self, version: Version) -> Result<Bytes> {
        if self.ndim() > 4 {
            return Err(Error::TooManyDimensions);
        }

        let mut data = BytesMut::with_capacity(0x1C);

        data.put_slice(b"PCBN");
        data.put_slice(version.data_version());
        data.put_u16_le(version.data_type_to_u16(self.data_type())?);
        data.put_u16_le(self.ndim() as u16);

        self.dimensions().try_for_each(|axis| -> Result<()> {
            data.put_u32_le(
                axis.len
                    .try_into()
                    .map_err(|_| Error::DataDimensionTooLarge)?,
            );
            Ok(())
        })?;

        (0..4 - self.ndim()).for_each(|_| data.put_u32_le(0));

        data.put(self.elements_to_array()?);

        Ok(data.freeze())
    }

    fn elements_to_array(&self) -> Result<Bytes> {
        let mut result: BytesMut;
        match self {
            DataContent::I8(array) => {
                let len = array.len();
                result = BytesMut::with_capacity(len);
                array.iter().for_each(|&e| result.put_i8(e));
            }
            DataContent::U8(array) => {
                let len = array.len();
                result = BytesMut::with_capacity(len);
                array.iter().for_each(|&e| result.put_u8(e));
            }
            DataContent::I16(array) => {
                let len = array.len();
                result = BytesMut::with_capacity(len * 2);
                array.iter().for_each(|&e| result.put_i16_le(e));
            }
            DataContent::U16(array) => {
                let len = array.len();
                result = BytesMut::with_capacity(len * 2);
                array.iter().for_each(|&e| result.put_u16_le(e));
            }
            DataContent::I32(array) => {
                let len = array.len();
                result = BytesMut::with_capacity(len * 4);
                array.iter().for_each(|&e| result.put_i32_le(e));
            }
            DataContent::F64(array) => {
                let len = array.len();
                result = BytesMut::with_capacity(len * 8);
                array.iter().for_each(|&e| result.put_f64_le(e));
            }
        }

        Ok(result.freeze())
    }

    pub fn supported_by(&self, version: Version) -> bool {
        self.minimum_version() >= version
    }
}

macro_rules! impl_into_data_content {
    ($ty:ident => $variant:ident) => {
        impl_into_data_content!(Array0 $ty => $variant);
        impl_into_data_content!(Array1 $ty => $variant);
        impl_into_data_content!(Array2 $ty => $variant);
        impl_into_data_content!(Array3 $ty => $variant);
        impl_into_data_content!(Array4 $ty => $variant);

        impl TryFrom<Data<$ty>> for DataContent {
            type Error = Error;

            fn try_from(value: Data<$ty>) -> Result<Self> {
                if value.ndim() > 4 {
                    Err(Error::TooManyDimensions)
                } else {
                    Ok(DataContent::$variant(value))
                }
            }
        }
    };

    ($container:ident $inner:ty => $variant:ident) => {
        impl From<$container<$inner>> for DataContent {
            fn from(value: $container<$inner>) -> Self {
                Self::$variant(value.into_dyn())
            }
        }
    }
}

impl_into_data_content!(i8 => I8);
impl_into_data_content!(u8 => U8);
impl_into_data_content!(i16 => I16);
impl_into_data_content!(u16 => U16);
impl_into_data_content!(i32 => I32);
impl_into_data_content!(f64 => F64);

trait DataElement: Sized {
    fn read<R: Buf>(reader: &mut R) -> Result<Self>;
    fn write<R: BufMut>(writer: &mut R) -> Result<()>;
}

impl FileType {
    pub const fn header_value(self, version: Version) -> u16 {
        match (version, self) {
            (_, FileType::Unknown(val)) => val,
            (_, FileType::Text) => 0x0,
            (_, FileType::Data) => 0x1,
            (Version::V1, FileType::Project) => 0x2,
        }
    }
}

#[derive(Clone, Debug)]
pub struct ProjectContent {
    files: HashMap<String, File>,
}

impl From<ProjectContent> for ContentType {
    fn from(value: ProjectContent) -> Self {
        ContentType::Project(value)
    }
}

impl FileContentType for ProjectContent {
    fn minimum_version(&self) -> Version {
        self.files
            .values()
            .map(|file| file.content.version())
            .max()
            .unwrap_or(Version::V1)
    }

    fn try_downgrade_to(self, version: Version) -> Result<Self> {
        if self.minimum_version() == version {
            return Ok(self);
        }

        let files = self
            .files
            .into_iter()
            .map(|(name, val)| val.try_downgrade_to(version).map(move |file| (name, file)))
            .collect::<Result<_>>()?;

        Ok(Self { files })
    }
}

impl ProjectContent {
    fn parse(mut reader: Bytes, version: Version, header: Option<&Header>) -> Result<Self> {
        need!(reader, 8);

        let _project_size = reader.get_u32_le() as usize;
        let file_count = reader.get_u32_le() as usize;

        need!(reader, (4 + version.file_name_len()) * file_count);

        let files = (0..file_count)
            .map(|_| {
                let project_size = reader.get_u32_le();
                let mut project_name = reader.copy_to_bytes(version.file_name_len());
                project_name.truncate(
                    project_name
                        .iter()
                        .position(|&b| b == b'\0')
                        .unwrap_or(version.file_name_len()),
                );

                (
                    project_size as usize,
                    String::from_utf8_lossy(&project_name).into_owned(),
                )
            })
            .collect::<Vec<_>>();

        Ok(ProjectContent {
            files: files
                .into_iter()
                .map(|(size, filename)| {
                    need!(reader, size);

                    let file = File::parse_with_inherit(&mut reader.split_to(size), header)?;
                    Ok((filename, file))
                })
                .collect::<Result<_>>()?,
        })
    }

    fn to_bytes(&self, version: Version, header: Option<&Header>) -> Result<Bytes> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use ndarray::array;

    use super::*;

    #[test]
    fn test_data_content_works() {
        let array = array![
            [[1, 2, 3], [4, 5, 6], [7, 8, 9]],
            [[10, 11, 12], [13, 14, 15], [16, 17, 18]],
            [[19, 20, 21], [22, 23, 24], [25, 26, 27]]
        ];

        let content = DataContent::from(array);
        let bytes = dbg!(content.to_bytes(Version::V1).expect("To bytes failed"));
        let parsed_content = DataContent::parse(bytes, Version::V1).expect("From bytes failed");
        assert_eq!(content.i32(), parsed_content.i32());
    }
}
