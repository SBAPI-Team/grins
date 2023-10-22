use bytes::{Buf, BufMut, Bytes, BytesMut};

use crate::{
    file::{File, Header, Version},
    Error, Result,
};
use std::collections::HashMap;

use super::{ContentType, FileContentType};

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
}

impl ProjectContent {
    pub(crate) fn parse(
        mut reader: Bytes,
        version: Version,
        header: Option<&Header>,
    ) -> Result<Self> {
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

    pub(crate) fn to_bytes(&self, version: Version, header: Option<&Header>) -> Result<Bytes> {
        let files = self
            .files
            .iter()
            .map(|(name, file)| {
                if name.len() > version.author_name_len() {
                    return Err(Error::FileNameTooLong);
                }
                file.to_bytes_inherit(header)
                    .map(|bytes| (name as &str, bytes))
            })
            .collect::<Result<Vec<_>>>()?;

        let header_len = 8 + (4 + version.file_name_len()) * files.len();
        let project_size = files.iter().map(|(_, bytes)| bytes.len()).sum();

        let mut header = BytesMut::with_capacity(header_len);
        let mut file_output = BytesMut::with_capacity(project_size);

        header.put_u32_le(project_size as u32);
        header.put_u32_le(files.len() as u32);

        for (name, bytes) in files {
            header.put_u32_le(bytes.len() as u32);
            header.put(name.as_bytes());
            header.put_bytes(0, version.author_name_len() - name.len());
            file_output.put(bytes);
        }

        let mut output = BytesMut::with_capacity(header_len + project_size);
        output.put(header);
        output.put(file_output);

        Ok(output.freeze())
    }
}
