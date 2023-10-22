#[macro_use]
extern crate thiserror;
#[macro_use]
mod util;

pub(crate) mod content;
pub(crate) mod file;

pub use content::*;
pub use file::*;

use bytes::Bytes;
use hmac::Mac;

pub type Sha1Hmac = hmac::Hmac<sha1::Sha1>;

pub fn calculate_hmac(inp: &[u8]) -> Bytes {
    const FOOTER_KEY: &[u8; 64] =
        br#"nqmby+e9S?{%U*-V]51n%^xZMk8>b{?x]&?(NmmV[,g85:%6Sqd"'U")/8u77UL2"#;

    Bytes::copy_from_slice(
        &Sha1Hmac::new_from_slice(&FOOTER_KEY[..])
            .expect("Creating HMAC instance")
            .chain_update(inp)
            .finalize()
            .into_bytes(),
    )
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("Unrecognized version {0}")]
    UnrecognizedVersion(u16),

    #[error("Not enough space in output buffer (need {0} more bytes)")]
    NotEnoughSpace(usize),

    #[error("Unexpected EOF (expected {0} more bytes)")]
    UnexpectedEof(usize),

    #[error("Invalid modification date in file")]
    InvalidHeaderModifyDate,

    #[error("Error while decompressing file: {0}")]
    DecompressError(#[from] miniz_oxide::inflate::DecompressError),

    #[error("File is too large (max size is 16MB)")]
    FileTooBig,

    #[error("Unrecognized file type {1} for {0}")]
    UnrecognizedFileType(Version, u16),

    #[error("Dimension is too large")]
    DataDimensionTooLarge,

    #[error("Too many dimensions")]
    TooManyDimensions,

    #[error("Invalid data header magic")]
    InvalidDataHeaderMagic,

    #[error("Mismatched data header version `{}` (expected `{}`)", String::from_utf8_lossy(.0), String::from_utf8_lossy(.1))]
    MismatchedDataVersion([u8; 4], [u8; 4]),

    #[error("Unrecognized type {1} in data file for {0}")]
    UnrecognizedDataType(Version, u16),

    #[error("Data type {1} is unsupported in {0}")]
    DataTypeUnsupported(Version, DataType),

    #[error("Filename in project is too long")]
    FileNameTooLong,
}

pub type Result<T> = std::result::Result<T, Error>;
