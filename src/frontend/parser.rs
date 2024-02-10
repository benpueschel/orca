use crate::{
    error::{Error, ErrorKind},
    frontend::ast::{
        BinaryExprData, FnDeclData, IfData, LetDeclData, Node, ProgramData, ReturnData,
    },
    frontend::lexer::{Lexer, Token},
};

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
        match $x.peek_token()? {
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

        while self.peek_token()? != Token::EOF {
            body.push(self.parse_declaration()?)
        }

        Ok(Node::Program(ProgramData { body }))
    }

    fn peek_token(&mut self) -> Result<Token, Error> {
        Ok(self.lexer.peek()?.0)
    }
    fn eat_token(&mut self) -> Result<Token, Error> {
        self.lexer.next_token()
    }

    fn parse_declaration(&mut self) -> NodeResult {
        // TODO: only function definitions are supported
        // declarations atm. structs, enums, etc. will
        // have to be implemented
        match self.peek_token()? {
            Token::Fn => return self.parse_fn_definition(),
            x => return Err(unexpected_token!(x)),
        }
    }

    fn parse_fn_definition(&mut self) -> NodeResult {
        verify_next_token!(self, Token::Fn);
        let _ = self.eat_token()?;

        verify_next_token!(self, Token::Identifier(_));
        let name = match self.eat_token()? {
            Token::Identifier(x) => x,
            _ => panic!("you seriously fucked up."),
        };
        verify_next_token!(self, Token::ParenOpen);
        let _ = self.eat_token()?;

        // TODO: parse function parameters

        verify_next_token!(self, Token::ParenClose);
        let _ = self.eat_token()?;

        // TODO: parse function return type

        verify_next_token!(self, Token::BracketOpen);
        let _ = self.eat_token()?;

        let body = self.parse_block()?;
        let _ = self.eat_token()?; // eat the closing bracket

        Ok(Node::FnDeclaration(FnDeclData { name, body }))
    }

    fn parse_block(&mut self) -> Result<Vec<Node>, Error> {
        let mut body = vec![];

        while self.peek_token()? != Token::BracketClose {
            let node = self.parse_statement()?;
            match node {
                Some(x) => body.push(x),
                None => {}
            }
        }
        Ok(body)
    }

    fn parse_statement(&mut self) -> Result<Option<Node>, Error> {
        let statement = match self.peek_token()? {
            Token::If => self.parse_if_statement(),
            Token::Return => self.parse_return_statement(),
            Token::Let => self.parse_variable_declaration(),
            _ => self.parse_expression(),
        };
        match statement {
            Ok(x) if x == Node::Semicolon => Ok(None),
            Ok(x) => Ok(Some(x)),
            Err(x) => Err(x),
        }
    }

    fn parse_if_statement(&mut self) -> NodeResult {
        verify_next_token!(self, Token::If);
        self.eat_token()?;
        verify_next_token!(self, Token::ParenOpen);
        self.eat_token()?;

        let expr = Box::new(self.parse_expression()?);

        verify_next_token!(self, Token::ParenClose);
        self.eat_token()?;
        verify_next_token!(self, Token::BracketOpen);
        self.eat_token()?;
        // series of statements

        let mut body = Vec::new();
        while self.peek_token()? != Token::BracketClose {
            if let Some(x) = self.parse_statement()? {
                body.push(x);
            }
        }

        verify_next_token!(self, Token::BracketClose);
        self.eat_token()?;
        // optional: else
        let mut else_body = Vec::new();
        if let Token::Else = self.peek_token()? {
            self.eat_token()?;
            verify_next_token!(self, Token::BracketOpen);
            self.eat_token()?;
            while self.peek_token()? != Token::BracketClose {
                if let Some(x) = self.parse_statement()? {
                    else_body.push(x);
                }
            }
            self.eat_token()?;
        }

        Ok(Node::IfStatement(IfData {
            expr,
            body,
            else_body,
        }))
    }

    fn parse_return_statement(&mut self) -> NodeResult {
        verify_next_token!(self, Token::Return);
        let _ = self.eat_token()?;

        let expr = match self.peek_token()? {
            Token::Semicolon => {
                let _ = self.eat_token();
                None
            }
            Token::Equal => return Err(unexpected_token!("Equal")),
            _ => Some(Box::new(self.parse_expression()?)),
        };

        Ok(Node::ReturnStatement(ReturnData { expr }))
    }

    fn parse_variable_declaration(&mut self) -> Result<Node, Error> {
        verify_next_token!(self, Token::Let);
        let _ = self.eat_token()?;

        verify_next_token!(self, Token::Identifier(_));
        let name = match self.eat_token()? {
            Token::Identifier(name) => name,
            _ => panic!("you seriously fucked up."),
        };

        match self.eat_token()? {
            // TODO: check types
            Token::Semicolon => {
                return Ok(Node::LetDeclaration(LetDeclData {
                    name,
                    expr: None,
                    r#type: None,
                }))
            }
            Token::Equal => match self.parse_add_expression()? {
                Node::Semicolon => Err(unexpected_token!("Semicolon")),
                node => Ok(Node::LetDeclaration(LetDeclData {
                    name,
                    expr: Some(Box::new(node)),
                    r#type: None,
                })),
            },
            x => Err(unexpected_token!(x)),
        }
    }

    fn parse_expression(&mut self) -> NodeResult {
        self.parse_assignment_expression()
    }

    fn parse_assignment_expression(&mut self) -> NodeResult {
        let mut left = self.parse_comparison_expression()?;
        if left == Node::Semicolon {
            return Ok(left);
        }
        while self.peek_token()? == Token::Equal {
            let operator = self.eat_token()?;
            let right = self.parse_comparison_expression()?;
            left = Node::BinaryExpr(BinaryExprData {
                left: Box::new(left),
                right: Box::new(right),
                operator,
            });
        }
        Ok(left)
    }

    fn parse_comparison_expression(&mut self) -> NodeResult {
        let mut left = self.parse_add_expression()?;
        if left == Node::Semicolon {
            return Ok(left);
        }
        while self.peek_token()? == Token::LeftCaret || self.peek_token()? == Token::RightCaret {
            let operator = self.eat_token()?;
            let right = self.parse_add_expression()?;
            left = Node::BinaryExpr(BinaryExprData {
                left: Box::new(left),
                right: Box::new(right),
                operator,
            });
        }
        Ok(left)
    }

    fn parse_add_expression(&mut self) -> NodeResult {
        let mut left = self.parse_mult_expression()?;
        if left == Node::Semicolon {
            return Ok(left);
        }
        while self.peek_token()? == Token::Plus || self.peek_token()? == Token::Minus {
            let operator = self.eat_token()?;
            let right = self.parse_mult_expression()?;
            left = Node::BinaryExpr(BinaryExprData {
                left: Box::new(left),
                right: Box::new(right),
                operator,
            });
        }
        Ok(left)
    }

    fn parse_mult_expression(&mut self) -> NodeResult {
        let mut left = self.parse_primary_expression()?;
        if left == Node::Semicolon {
            return Ok(left);
        }
        while self.peek_token()? == Token::Star || self.peek_token()? == Token::Slash {
            let operator = self.eat_token()?;
            let right = self.parse_primary_expression()?;
            left = Node::BinaryExpr(BinaryExprData {
                left: Box::new(left),
                right: Box::new(right),
                operator,
            });
        }
        Ok(left)
    }

    fn parse_primary_expression(&mut self) -> NodeResult {
        let token = self.eat_token()?;
        match token {
            Token::Identifier(x) => Ok(Node::Identifier(x)),
            Token::Integer(x) => Ok(Node::IntegerLiteral(x)),
            Token::ParenOpen => self.parse_paren_expression(),
            Token::Semicolon => Ok(Node::Semicolon),
            x => Err(unexpected_token!(x)),
        }
    }

    fn parse_paren_expression(&mut self) -> NodeResult {
        // FIXME: not validating ParenOpen token, could lead to nasty bugs
        let value = self.parse_expression();
        verify_next_token!(self, Token::ParenClose);
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
