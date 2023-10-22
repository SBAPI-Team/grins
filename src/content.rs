use crate::file::{FileType, Header, Version};
use crate::{Error, Result};

mod data;
mod project;

use bytes::Bytes;
pub use data::*;
pub use project::*;

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

    pub fn to_bytes(&self, header: Option<&Header>) -> Result<Bytes> {
        match self {
            Content::Unparsed(_, _, bytes) => Ok(bytes.clone()),
            Content::Parsed(version, file) => file.to_bytes(*version, header),
        }
    }
}

pub trait FileContentType: Into<ContentType> {
    fn minimum_version(&self) -> Version;

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

    pub fn to_bytes(&self, version: Version, header: Option<&Header>) -> Result<Bytes> {
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
}
