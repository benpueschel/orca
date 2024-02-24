use crate::{
    error::{Error, ErrorKind},
    frontend::{
        ast::{BinaryExprData, FnDeclData, IfData, LetDeclData, Node, ProgramData, ReturnData},
        lexer::{Lexer, Token},
    },
    span::Span,
};

use super::{
    ast::{IdentifierData, NodeType},
    lexer::TokenType,
};

use super::ast::Type;

macro_rules! unexpected_token {
    ($x:tt) => {
        Error::new(
            ErrorKind::UnexpectedToken,
            format!("unexpected token {:?}.", $x),
        )
    };
}

macro_rules! verify_next_token {
    ($x:tt, $pattern:pat $(,)?) => {
        match $x.peek_token()?.token_type {
            $pattern => {}
            x => {
                return Err(Error::new(
                    ErrorKind::UnexpectedToken,
                    format!(
                        "unexpected token {:?}. expected {:?}",
                        x,
                        stringify!($pattern)
                    ),
                ));
            }
        }
    };
}

pub struct Parser {
    lexer: Lexer,
}

type NodeResult = Result<Node, crate::error::Error>;

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Parser { lexer }
    }

    pub fn produce_ast(&mut self) -> NodeResult {
        let mut body: Vec<Node> = vec![];

        while self.peek_token()?.token_type != TokenType::EOF {
            body.push(self.parse_declaration()?)
        }

        let empty_node = Node {
            node_type: NodeType::Semicolon,
            span: Span::empty(),
        };
        let span = Span::compose(
            body.first().unwrap_or(&empty_node).span,
            body.first().unwrap_or(&empty_node).span,
        );

        Ok(Node {
            node_type: NodeType::Program(ProgramData { body }),
            span,
        })
    }

    fn peek_token(&mut self) -> Result<Token, Error> {
        Ok(self.lexer.peek()?)
    }
    fn eat_token(&mut self) -> Result<Token, Error> {
        self.lexer.next_token()
    }

    fn parse_declaration(&mut self) -> NodeResult {
        // TODO: only function definitions are supported
        // declarations atm. structs, enums, etc. will
        // have to be implemented
        match self.peek_token()?.token_type {
            TokenType::Fn => return self.parse_fn_definition(),
            x => return Err(unexpected_token!(x)),
        }
    }

    fn parse_fn_definition(&mut self) -> NodeResult {
        verify_next_token!(self, TokenType::Fn);
        let first_token = self.eat_token()?;

        verify_next_token!(self, TokenType::Identifier(_));
        let name = match self.eat_token()?.token_type {
            TokenType::Identifier(x) => x,
            _ => panic!("you seriously fucked up."),
        };
        verify_next_token!(self, TokenType::ParenOpen);
        let _ = self.eat_token()?;

        // TODO: parse function parameters

        verify_next_token!(self, TokenType::ParenClose);
        let _ = self.eat_token()?;

        let return_type = if let TokenType::Colon = self.peek_token()?.token_type {
            self.eat_token()?;
            Some(self.parse_type()?)
        } else {
            None
        };

        verify_next_token!(self, TokenType::BracketOpen);
        let _ = self.eat_token()?;

        let body = self.parse_block()?;
        let last_token = self.eat_token()?; // eat the closing bracket

        Ok(Node {
            node_type: NodeType::FnDeclaration(FnDeclData { name, body, return_type }),
            span: Span::compose(first_token.span, last_token.span),
        })
    }

    fn parse_type(&mut self) -> Result<Type, Error> {
        let ident = match self.eat_token()?.token_type {
            TokenType::Identifier(x) => x,
            x => {
                return Err(Error::new(
                    ErrorKind::UnexpectedToken,
                    format!("unexpected token {:?}. expected identifier", x),
                ));
            }
        };

        Ok(match ident.as_ref() {
            "usize" => Type::Usize,
            "u32" => Type::U32,
            _ => Type::Identifier(ident),
        })
    }

    fn parse_block(&mut self) -> Result<Vec<Node>, Error> {
        let mut body = vec![];

        while self.peek_token()?.token_type != TokenType::BracketClose {
            let node = self.parse_statement()?;
            match node {
                Some(x) => body.push(x),
                None => {}
            }
        }
        Ok(body)
    }

    fn parse_statement(&mut self) -> Result<Option<Node>, Error> {
        let statement = match self.peek_token()?.token_type {
            TokenType::If => self.parse_if_statement(),
            TokenType::Return => self.parse_return_statement(),
            TokenType::Let => self.parse_variable_declaration(),
            _ => self.parse_expression(),
        };
        match statement {
            Ok(x) if x.node_type == NodeType::Semicolon => Ok(None),
            Ok(x) => Ok(Some(x)),
            Err(x) => Err(x),
        }
    }

    fn parse_if_statement(&mut self) -> NodeResult {
        verify_next_token!(self, TokenType::If);
        let first_token = self.eat_token()?;
        verify_next_token!(self, TokenType::ParenOpen);
        self.eat_token()?;

        let expr = Box::new(self.parse_expression()?);

        verify_next_token!(self, TokenType::ParenClose);
        self.eat_token()?;
        verify_next_token!(self, TokenType::BracketOpen);
        self.eat_token()?;
        // series of statements

        let mut body = Vec::new();
        while self.peek_token()?.token_type != TokenType::BracketClose {
            if let Some(x) = self.parse_statement()? {
                body.push(x);
            }
        }

        verify_next_token!(self, TokenType::BracketClose);
        let mut last_token = self.eat_token()?;
        // optional: else
        let mut else_body = Vec::new();
        if let TokenType::Else = self.peek_token()?.token_type {
            self.eat_token()?;
            verify_next_token!(self, TokenType::BracketOpen);
            self.eat_token()?;
            while self.peek_token()?.token_type != TokenType::BracketClose {
                if let Some(x) = self.parse_statement()? {
                    else_body.push(x);
                }
            }
            last_token = self.eat_token()?;
        }

        Ok(Node {
            node_type: NodeType::IfStatement(IfData {
                expr,
                body,
                else_body,
            }),
            span: Span::compose(first_token.span, last_token.span),
        })
    }

    fn parse_return_statement(&mut self) -> NodeResult {
        verify_next_token!(self, TokenType::Return);
        let first_token = self.eat_token()?;

        let last_pos;
        let expr = match self.peek_token()?.token_type {
            TokenType::Semicolon => {
                last_pos = self.eat_token()?.span;
                None
            }
            TokenType::Equal => return Err(unexpected_token!("Equal")),
            _ => {
                let expr = self.parse_expression()?;
                last_pos = expr.span;
                Some(Box::new(expr))
            }
        };

        Ok(Node {
            node_type: NodeType::ReturnStatement(ReturnData {
                expr,
                fn_name: "main".into(), // TODO: find function name
            }),
            span: Span::compose(first_token.span, last_pos),
        })
    }

    fn parse_variable_declaration(&mut self) -> Result<Node, Error> {
        verify_next_token!(self, TokenType::Let);
        let first_token = self.eat_token()?;

        verify_next_token!(self, TokenType::Identifier(_));
        let name = match self.eat_token()?.token_type {
            TokenType::Identifier(name) => name,
            _ => panic!("you seriously fucked up."),
        };

        let last_token = self.eat_token()?;
        match last_token.token_type {
            // TODO: check types
            TokenType::Colon => {
                let r#type = Some(self.parse_type()?);
                let expr = if let TokenType::Equal = self.peek_token()?.token_type {
                    self.eat_token()?;
                    Some(Box::new(self.parse_expression()?))
                } else {
                    None
                };
                return Ok(Node {
                    span: Span::compose(first_token.span, last_token.span),
                    node_type: NodeType::LetDeclaration(LetDeclData { name, expr, r#type }),
                });
            }
            TokenType::Semicolon => {
                return Ok(Node { 
                    span: Span::compose(first_token.span, last_token.span), 
                    node_type: NodeType::LetDeclaration(LetDeclData {
                        name,
                        expr: None,
                        r#type: None,
                    }),
                })
            }
            x => Err(unexpected_token!(x)),
        }
    }

    fn parse_expression(&mut self) -> NodeResult {
        self.parse_assignment_expression()
    }

    fn parse_assignment_expression(&mut self) -> NodeResult {
        let mut left = self.parse_comparison_expression()?;
        if let NodeType::Semicolon = left.node_type {
            return Ok(left);
        }
        while self.peek_token()?.token_type == TokenType::Equal {
            let operator = self.eat_token()?;
            let right = self.parse_comparison_expression()?;
            let span = Span::compose(left.span, right.span);
            left = Node {
                node_type: NodeType::BinaryExpr(BinaryExprData {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator,
                }),
                span,
            };
        }
        Ok(left)
    }

    fn parse_comparison_expression(&mut self) -> NodeResult {
        let mut left = self.parse_add_expression()?;
        if let NodeType::Semicolon = left.node_type {
            return Ok(left);
        }
        while self.peek_token()?.token_type == TokenType::LeftCaret
            || self.peek_token()?.token_type == TokenType::RightCaret
        {
            let operator = self.eat_token()?;
            let right = self.parse_add_expression()?;
            let span = Span::compose(left.span, right.span);
            left = Node {
                node_type: NodeType::BinaryExpr(BinaryExprData {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator,
                }),
                span,
            };
        }
        Ok(left)
    }

    fn parse_add_expression(&mut self) -> NodeResult {
        let mut left = self.parse_mult_expression()?;
        if let NodeType::Semicolon = left.node_type {
            return Ok(left);
        }
        while self.peek_token()?.token_type == TokenType::Plus
            || self.peek_token()?.token_type == TokenType::Minus
        {
            let operator = self.eat_token()?;
            let right = self.parse_mult_expression()?;
            let span = Span::compose(left.span, right.span);
            left = Node {
                node_type: NodeType::BinaryExpr(BinaryExprData {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator,
                }),
                span,
            };
        }
        Ok(left)
    }

    fn parse_mult_expression(&mut self) -> NodeResult {
        let mut left = self.parse_primary_expression()?;
        if let NodeType::Semicolon = left.node_type {
            return Ok(left);
        }
        while self.peek_token()?.token_type == TokenType::Star
            || self.peek_token()?.token_type == TokenType::Slash
        {
            let operator = self.eat_token()?;
            let right = self.parse_primary_expression()?;
            let span = Span::compose(left.span, right.span);
            left = Node {
                node_type: NodeType::BinaryExpr(BinaryExprData {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator,
                }),
                span,
            };
        }
        Ok(left)
    }

    fn parse_primary_expression(&mut self) -> NodeResult {
        let token = self.eat_token()?;
        match token.token_type {
            TokenType::Identifier(x) => Ok(Node {
                node_type: NodeType::Identifier(IdentifierData { name: x }),
                span: token.span,
            }),
            TokenType::Integer(x) => Ok(Node {
                node_type: NodeType::IntegerLiteral(x),
                span: token.span,
            }),
            TokenType::ParenOpen => self.parse_paren_expression(),
            TokenType::Semicolon => Ok(Node {
                node_type: NodeType::Semicolon,
                span: token.span,
            }),
            x => Err(unexpected_token!(x)),
        }
    }

    fn parse_paren_expression(&mut self) -> NodeResult {
        // FIXME: not validating ParenOpen token, could lead to nasty bugs
        let value = self.parse_expression();
        verify_next_token!(self, TokenType::ParenClose);
        let _ = self.eat_token()?;
        value
    }
}

#[cfg(test)]
mod test {
    use super::Parser;
    use crate::frontend::lexer::Lexer;

    #[test]
    fn test() {
        let lexer = Lexer::new(
            "fn main() { 
                let x = 3 + 7; 
                x = x * 5;
            }"
            .into(),
        );
        let mut parser = Parser::new(lexer);
        let ast = parser.produce_ast().expect("parsing error");
        println!("{:?}", ast);
    }
}
