use std::fmt::Display;

use bytes::{Buf, BufMut, Bytes, BytesMut};
use ndarray::{iter::Axes, Array, Array0, Array1, Array2, Array3, Array4, IxDyn};

use crate::{file::Version, Error, Result};

use super::{ContentType, FileContentType};

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

    pub fn ndim(&self) -> usize {
        match self {
            DataContent::I8(array) => array.ndim(),
            DataContent::U8(array) => array.ndim(),
            DataContent::I16(array) => array.ndim(),
            DataContent::U16(array) => array.ndim(),
            DataContent::I32(array) => array.ndim(),
            DataContent::F64(array) => array.ndim(),
        }
    }

    pub fn axes(&self) -> Axes<'_, IxDyn> {
        match self {
            DataContent::I8(array) => array.axes(),
            DataContent::U8(array) => array.axes(),
            DataContent::I16(array) => array.axes(),
            DataContent::U16(array) => array.axes(),
            DataContent::I32(array) => array.axes(),
            DataContent::F64(array) => array.axes(),
        }
    }

    pub(crate) fn to_bytes(&self, version: Version) -> Result<Bytes> {
        if self.ndim() > 4 {
            return Err(Error::TooManyDimensions);
        }

        let mut data = BytesMut::with_capacity(0x1C);

        data.put_slice(b"PCBN");
        data.put_slice(version.data_version());
        data.put_u16_le(version.data_type_to_u16(self.data_type())?);
        data.put_u16_le(self.ndim() as u16);

        self.axes().try_for_each(|axis| -> Result<()> {
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
