#[derive(Debug)]
pub enum ErrorKind {
    InvalidData,
    UnexpectedEof,
    UnexpectedSymbol,
    RegisterOverflow,

    UnexpectedToken,
    UnexpectedNone,
}

impl ErrorKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::InvalidData => "invalid data.",
            Self::RegisterOverflow => "register overflow.",
            Self::UnexpectedEof => "unexpected end of file.",
            Self::UnexpectedSymbol => "unexpected symbol.",
            Self::UnexpectedToken => "unexpected token.",
            Self::UnexpectedNone => "unexpected None.",
        }
    }
}

#[macro_export]
macro_rules! option_unwrap {
    ($expr:expr, $err_msg:tt) => {
        match $expr {
            Some(x) => x,
            None => return Err(Error::new(ErrorKind::UnexpectedNone, $err_msg))
        }
    };
}

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    msg: String,
}

impl From<ErrorKind> for Error {
    fn from(val: ErrorKind) -> Self {
        let str = val.as_str();
        Error::new(val, str)
    }
}

impl Error {
    pub fn new<E>(kind: ErrorKind, error: E) -> Error
    where
        E: Into<String>,
    {
        Error {
            kind,
            msg: error.into(),
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}: {}", self.kind, self.msg)
    }
}

impl std::error::Error for Error {}
