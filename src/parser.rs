use crate::{
    error::{Error, ErrorKind},
    lexer::{Lexer, Token},
};

// TODO: rethink Box<Node> - storing nodes on the heap isn't great.
// use lifetimed reference?
#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Program {
        body: Vec<Node>,
    },
    BinaryExpr {
        left: Box<Node>,
        right: Box<Node>,
        operator: Token,
    },
    Identifier {
        value: String,
    },
    IntegerLiteral {
        value: usize,
    },
    Semicolon,
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
            let node = self.parse_statement()?;
            match node {
                Some(x) => body.push(x),
                None => {}
            }
        }

        Ok(Node::Program { body })
    }

    fn peek_token(&mut self) -> Result<Token, Error> {
        Ok(self.lexer.peek()?.0)
    }
    fn eat_token(&mut self) -> Result<Token, Error> {
        self.lexer.next_token()
    }

    fn parse_statement(&mut self) -> Result<Option<Node>, Error> {
        match self.parse_expression() {
            Ok(x) if x == Node::Semicolon => Ok(None),
            Ok(x) => Ok(Some(x)),
            Err(x) => Err(x),
        }
    }

    fn parse_expression(&mut self) -> NodeResult {
        self.parse_assignment_expression()
    }

    fn parse_assignment_expression(&mut self) -> NodeResult {
        let mut left = self.parse_add_expression()?;
        if left == Node::Semicolon {
            return Ok(left);
        }
        while self.peek_token()? == Token::Equal {
            let operator = self.eat_token()?;
            let right = self.parse_add_expression()?;
            left = Node::BinaryExpr {
                left: Box::new(left),
                right: Box::new(right),
                operator,
            };
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
            left = Node::BinaryExpr {
                left: Box::new(left),
                right: Box::new(right),
                operator,
            };
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
            left = Node::BinaryExpr {
                left: Box::new(left),
                right: Box::new(right),
                operator,
            };
        }
        Ok(left)
    }

    fn parse_primary_expression(&mut self) -> NodeResult {
        let token = self.eat_token()?;
        match token {
            Token::Identifier(x) => Ok(Node::Identifier { value: x }),
            Token::Integer(x) => Ok(Node::IntegerLiteral { value: x }),
            Token::ParenOpen => self.parse_paren_expression(),
            Token::Semicolon => Ok(Node::Semicolon),
            _ => Err(Error::new(
                ErrorKind::UnexpectedToken,
                format!("unexpected token {:?}", token),
            )),
        }
    }

    fn parse_paren_expression(&mut self) -> NodeResult {
        let value = self.parse_expression();
        let next_token = self.peek_token()?;
        match next_token {
            Token::ParenClose => {
                // consume the closing paren
                let _ = self.eat_token();
            }
            x => {
                return Err(Error::new(
                    ErrorKind::UnexpectedToken,
                    format!("unexpected token '{:?}'. Expected ParenClose ')'", x),
                ))
            }
        };
        value
    }
}

#[cfg(test)]
mod test {
    use super::Parser;
    use crate::lexer::Lexer;

    #[test]
    fn test() {
        let lexer = Lexer::new("".into());
        let mut parser = Parser::new(lexer);
        let ast = parser.produce_ast();
        println!("{:?}", ast);
    }
}
