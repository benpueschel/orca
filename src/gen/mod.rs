use crate::ast::Node;
use crate::{error::Error, lexer::Token};

use self::x86_linux::LinuxX86Asm;

pub mod x86_linux;

pub type Register = u8;

pub enum TargetPlatform {
    LinuxX86_64,
}

pub fn create_code_generator<'a>(target: TargetPlatform) -> Box<dyn CodeGenerator<'a>> {
    match target {
        TargetPlatform::LinuxX86_64 => Box::new(LinuxX86Asm::new()),
    }
}

pub trait CodeGenerator<'a> {
    fn generate_assembly(&mut self, node: &'a Node) -> Result<&str, Error>;
}

struct GenNode<'a> {
    pub node: &'a Node,
    pub register: Option<Register>,
}

trait Assembly {
    fn register_alloc(&mut self) -> Result<Register, Error>;
    fn register_free(&mut self, reg: Register);
    fn register_name(reg: Register) -> &'static str;

    fn label_create(&mut self) -> usize;
    fn label_name(label: usize) -> String;
    fn instruction_name(operator: &Token) -> &'static str;
}

#[cfg(test)]
mod test {
    use crate::{
        gen::{create_code_generator, TargetPlatform::LinuxX86_64},
        lexer::Lexer,
        parser::Parser,
    };

    #[test]
    fn test() {
        let lexer = Lexer::new(
            "fn main() { 
                let x = 0; 
                let y = 5 * x;

                if(y - x) {
                    y = 3;
                } else {
                    y = 6;
                }

                return y - 3;
            }"
            .into(),
        );
        let mut parser = Parser::new(lexer);
        let ast = parser.produce_ast().expect("could not produce ast");
        let mut generator = create_code_generator(LinuxX86_64);

        match generator.generate_assembly(&ast) {
            Ok(x) => println!("{}", x),
            Err(x) => panic!("{:?}", x),
        };

        panic!();
    }
}
