use crate::{
    error::{Error, ErrorKind},
    span::Span,
};
use std::ops::Add;

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
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
    LeftCaret,
    RightCaret,
    ParenOpen,
    ParenClose,
    BracketOpen,
    BracketClose,
    Semicolon,
    EOF,
    Invalid,
}

#[derive(Debug, PartialEq, Clone)]
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

    pub fn peek(&self) -> Result<Token, LexerError> {
        let whitespaces = self.count_whitespaces();
        let i = self.current_pos + whitespaces;
        let next = match self.input[i..].chars().next() {
            None => {
                return Ok(Token {
                    token_type: TokenType::EOF,
                    span: Span::new(i, i),
                })
            }
            Some(x) => x,
        };

        let result = match next {
            ';' => Ok(Token {
                token_type: TokenType::Semicolon,
                span: Span::single(i),
            }),
            '=' => Ok(Token {
                token_type: TokenType::Equal,
                span: Span::single(i),
            }),
            '(' => Ok(Token {
                token_type: TokenType::ParenOpen,
                span: Span::single(i),
            }),
            ')' => Ok(Token {
                token_type: TokenType::ParenClose,
                span: Span::single(i),
            }),
            '{' => Ok(Token {
                token_type: TokenType::BracketOpen,
                span: Span::single(i),
            }),
            '}' => Ok(Token {
                token_type: TokenType::BracketClose,
                span: Span::single(i),
            }),
            '*' => Ok(Token {
                token_type: TokenType::Star,
                span: Span::single(i),
            }),
            '%' => Ok(Token {
                token_type: TokenType::Percent,
                span: Span::single(i),
            }),
            '/' => Ok(Token {
                token_type: TokenType::Slash,
                span: Span::single(i),
            }),
            '+' => Ok(Token {
                token_type: TokenType::Plus,
                span: Span::single(i),
            }),
            '-' => Ok(Token {
                token_type: TokenType::Minus,
                span: Span::single(i),
            }),
            '<' => Ok(Token {
                token_type: TokenType::LeftCaret,
                span: Span::single(i),
            }),
            '>' => Ok(Token {
                token_type: TokenType::RightCaret,
                span: Span::single(i),
            }),

            c if c >= '0' && c <= '9' => self.tokenize_number(i),
            c @ '_' | c if c.is_alphabetic() => self.tokenize_ident(i),

            x => Err(Error::new(
                ErrorKind::UnexpectedSymbol,
                format!("unexpected character {}", x),
            )),
        }?;
        Ok(result)
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        let token = self.peek()?;

        self.current_pos = token.span.end;
        return Ok(token);
    }

    pub fn tokenize_ident(&self, pos: usize) -> Result<Token, LexerError> {
        // identifiers need to start with an alphabetic ascii char
        let data = &self.input[pos..];
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
            return Ok(Token {
                token_type: TokenType::Identifier(identifier.to_string()),
                span: Span::single(pos),
            });
        }

        Ok(Token {
            token_type: match identifier {
                "return" => TokenType::Return,
                "let" => TokenType::Let,
                "fn" => TokenType::Fn,
                "if" => TokenType::If,
                "else" => TokenType::Else,
                _ => TokenType::Identifier(identifier.to_string()),
            },
            span: Span::with_len(pos, identifier.len()),
        })
    }

    fn tokenize_number(&self, start: usize) -> Result<Token, LexerError> {
        let mut seen_dot = false;
        let data = &self.input[start..];

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
            Ok(Token {
                token_type: TokenType::Float(value),
                span: Span::with_len(start, i),
            })
        } else {
            let value: usize = match decimal.parse() {
                Ok(x) => x,
                Err(_) => {
                    return Err(ErrorKind::InvalidData.into());
                }
            };
            Ok(Token {
                token_type: TokenType::Integer(value),
                span: Span::with_len(start, i),
            })
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
            Ok(x) => match x.token_type {
                TokenType::EOF => None,
                _ => Some(x),
            },
            Err(x) => panic!("{:?}", x),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Lexer, TokenType};
    macro_rules! token_eq {
        { $x:expr,$y:expr } => {
            match $x {
                Ok(x) => assert_eq!(x.token_type, $y),
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
        test_token!("fn", TokenType::Fn);
        test_token!("let", TokenType::Let);
        test_token!("return", TokenType::Return);
        // go through the alphabet and test single characters
        for c in 'a'..='z' {
            test_token!(c, TokenType::Identifier(c.into()));
        }
        // go through all digits and test chars
        for c in 0..=9 {
            test_token!((c + '0' as u8) as char, TokenType::Integer(c.into()));
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
                TokenType::Fn,
                TokenType::Identifier("main".into()),
                TokenType::ParenOpen,
                TokenType::ParenClose,
                TokenType::BracketOpen,
                TokenType::Let,
                TokenType::Identifier("x".into()),
                TokenType::Equal,
                TokenType::Integer(1),
                TokenType::Semicolon,
                TokenType::Return,
                TokenType::Identifier("x".into()),
                TokenType::Semicolon,
                TokenType::BracketClose,
            )
        );
    }
}
