use std::io::Write;

use crate::{lexer::Lexer, parser::Parser};

pub mod error;
pub mod lexer;
pub mod parser;
pub mod ast;
pub mod gen;

fn main() -> std::io::Result<()> {
    let stdin = std::io::stdin();
    let mut stdout = std::io::stdout();
    let mut buf = String::new();
    loop {
        print!("> ");
        stdout.flush()?;
        buf.clear();
        stdin.read_line(&mut buf)?;

        let lexer = Lexer::new(buf.clone());
        println!("{:?}", lexer.peek());
        let mut parser = Parser::new(lexer);
        let program = parser.produce_ast();
        println!("{:?}", program);
    }
}
