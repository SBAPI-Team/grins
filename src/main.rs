use std::{fs::File, io::Read};

use bytes::{Buf, Bytes};
use grins::{header::File as SBFile, Error};

fn main() -> anyhow::Result<()> {
    for arg in std::env::args().skip(1) {
        let mut file = File::open(arg)?;
        let mut file_contents = vec![];
        file.read_to_end(&mut file_contents)?;

        let file = match SBFile::parse(file_contents) {
            Ok(file) => file,
            Err(Error::UnrecognizedVersion(_)) => continue,
            err => err?,
        };

        dbg!(file.header());
        dbg!(file.content().version());
        dbg!(file.content().file_type());

        let content = file.content().clone();
        let c = content.into_parsed(Some(file.header()))?;
        dbg!(c);
    }

    Ok(())
}
