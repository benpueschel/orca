use crate::error::{Error, ErrorKind};
use std::ops::Add;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Return,
    Fn,
    Let,
    If,
    Else,
    Integer(usize),
    Float(f64),
    Identifier(String),
    Equal,
    Star,
    Percent,
    Slash,
    Plus,
    Minus,
    ParenOpen,
    ParenClose,
    BracketOpen,
    BracketClose,
    Semicolon,
    EOF,
    Invalid,
}

pub struct Lexer {
    input: String,
    current_pos: usize,
}

type LexerError = Error;

impl Lexer {
    pub fn new(input: String) -> Self {
        Lexer {
            current_pos: 0,
            input: input.add("\n"),
        }
    }

    pub fn peek(&self) -> Result<(Token, usize), LexerError> {
        let whitespaces = self.count_whitespaces();
        let i = self.current_pos + whitespaces;
        let slice = &self.input[i..];
        let next = match slice.chars().next() {
            None => return Ok((Token::EOF, 0)),
            Some(x) => x,
        };

        let result = match next {
            ';' => Ok((Token::Semicolon, 1)),
            '=' => Ok((Token::Equal, 1)),
            '(' => Ok((Token::ParenOpen, 1)),
            ')' => Ok((Token::ParenClose, 1)),
            '{' => Ok((Token::BracketOpen, 1)),
            '}' => Ok((Token::BracketClose, 1)),
            '*' => Ok((Token::Star, 1)),
            '%' => Ok((Token::Percent, 1)),
            '/' => Ok((Token::Slash, 1)),
            '+' => Ok((Token::Plus, 1)),
            '-' => Ok((Token::Minus, 1)),

            c if c >= '0' && c <= '9' => Self::tokenize_number(slice),
            c @ '_' | c if c.is_alphabetic() => Self::tokenize_ident(slice),

            x => Err(Error::new(
                ErrorKind::UnexpectedSymbol,
                format!("unexpected character {}", x),
            )),
        }?;
        Ok((result.0, result.1 + whitespaces))
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        let (token, length) = self.peek()?;

        self.current_pos += length;
        return Ok(token);
    }

    pub fn tokenize_ident(data: &str) -> Result<(Token, usize), LexerError> {
        // identifiers need to start with an alphabetic ascii char
        match data.chars().next() {
            Some(x) if !x.is_alphabetic() => {
                return Err(Error::new(
                    ErrorKind::InvalidData,
                    "identifier start was not alphabetical",
                ));
            }
            None => {
                return Err(ErrorKind::UnexpectedEof.into());
            }
            _ => {}
        };

        let mut i = 0;
        let mut skip_keyword_val = false;
        for c in data.chars() {
            i += 1;
            if c == '_' {
                // NOTE: no keyword contains '_', so we can skip validation
                skip_keyword_val = true;
                continue;
            }
            if !c.is_alphanumeric() {
                break;
            }
        }
        // FIXME: ugly as hell
        i -= 1;

        let identifier = &data[0..i];
        if skip_keyword_val {
            return Ok((Token::Identifier(identifier.to_string()), i));
        }

        Ok((
            match identifier {
                "return" => Token::Return,
                "let" => Token::Let,
                "fn" => Token::Fn,
                "if" => Token::If,
                "else" => Token::Else,
                _ => Token::Identifier(identifier.to_string()),
            },
            identifier.len(),
        ))
    }

    fn tokenize_number(data: &str) -> Result<(Token, usize), LexerError> {
        let mut seen_dot = false;

        match data.chars().next() {
            Some(x) if !x.is_numeric() => {
                return Err(Error::new(
                    ErrorKind::InvalidData,
                    "number must start with numerical value [0-9]",
                ));
            }
            None => {
                return Err(ErrorKind::UnexpectedEof.into());
            }
            _ => {}
        };

        let mut i = 0;
        for c in data.chars() {
            i += 1;

            if c == '.' {
                if seen_dot {
                    break;
                }
                seen_dot = true;
            }

            if !c.is_numeric() {
                break;
            }
        }
        // FIXME: ugly as hell
        i -= 1;

        let decimal = &data[0..i];

        if seen_dot {
            let value: f64 = match decimal.parse() {
                Ok(x) => x,
                Err(_) => {
                    return Err(ErrorKind::InvalidData.into());
                }
            };
            Ok((Token::Float(value), i))
        } else {
            let value: usize = match decimal.parse() {
                Ok(x) => x,
                Err(_) => {
                    return Err(ErrorKind::InvalidData.into());
                }
            };
            Ok((Token::Integer(value), i))
        }
    }

    fn count_whitespaces(&self) -> usize {
        let mut i = 0;
        for c in self.input[self.current_pos..].chars() {
            if !c.is_whitespace() {
                break;
            }
            i += 1;
        }
        i
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(x) => Some(x),
            Err(x) => panic!("{:?}", x),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::{Lexer, Token};
    macro_rules! token_eq {
        { $x:expr,$y:expr } => {
            match $x {
                Ok(x) => assert_eq!(x, $y),
                Err(x) => panic!("{:?}", x),
            }
        };
    }
    macro_rules! test_token {
        { $x:expr,$y:expr } => {
            let mut lexer = Lexer::new($x.into());
            token_eq!(lexer.next_token(), $y);
        };
    }
    macro_rules! test_tokens {
        { $x:expr,$y:expr } => {
            let mut lexer = Lexer::new($x.into());
            for token in $y {
                token_eq!(lexer.next_token(), token);
            }
        }
    }

    #[test]
    fn test_keywords() {
        test_token!("fn", Token::Fn);
        test_token!("let", Token::Let);
        test_token!("return", Token::Return);
        // go through the alphabet and test single characters
        for c in 'a'..='z' {
            test_token!(c, Token::Identifier(c.into()));
        }
        // go through all digits and test chars
        for c in 0..=9 {
            test_token!((c + '0' as u8) as char, Token::Integer(c.into()));
        }
    }

    #[test]
    fn test_program() {
        test_tokens!(
            "fn main() {\n
                let x = 1;\n
                return x;\n
            }",
            vec!(
                Token::Fn,
                Token::Identifier("main".into()),
                Token::ParenOpen,
                Token::ParenClose,
                Token::BracketOpen,
                Token::Let,
                Token::Identifier("x".into()),
                Token::Equal,
                Token::Integer(1),
                Token::Semicolon,
                Token::Return,
                Token::Identifier("x".into()),
                Token::Semicolon,
                Token::BracketClose,
            )
        );
    }
}
