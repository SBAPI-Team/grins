use hmac::Mac;

#[macro_export]
macro_rules! need {
    ($buf:ident, $count:expr) => {
        if $buf.remaining() < $count {
            return Err(Error::UnexpectedEof($count - $buf.remaining()));
        }
    };

    (mut $buf:ident, $count:expr) => {
        if $buf.remaining_mut() < $count {
            return Err(Error::NotEnoughSpace($count - $buf.remaining_mut()));
        }
    };
}
