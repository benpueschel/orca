use std::collections::{HashMap, VecDeque};

use crate::{
    error::{Error, ErrorKind},
    gen::Assembly,
    lexer::Token,
    option_unwrap,
    parser::Node,
};

use super::{CodeGenerator, GenNode, Register};

const REGISTER_SIZE: usize = 6;

pub struct LinuxX86Asm {
    registers: [bool; REGISTER_SIZE],
    output: String,
    stack_offset: usize,
    idents_in_scope: VecDeque<HashMap<String, String>>,
}

impl<'a> CodeGenerator<'a> for LinuxX86Asm {
    fn generate_assembly(&mut self, node: &'a Node) -> Result<&str, Error> {
        match node {
            Node::Program { body } => {
                self.idents_in_scope.push_front(HashMap::new());
                for node in body {
                    let _ = self.node_gen(node);
                }
                self.idents_in_scope.pop_back();
                return Ok(&self.output);
            }
            _ => return Err(Error::new(ErrorKind::InvalidData, "node is not a program.")),
        }
    }
}

impl<'a> LinuxX86Asm {
    pub fn new() -> Self {
        LinuxX86Asm {
            registers: [false; REGISTER_SIZE],
            output: String::new(),
            stack_offset: 0,
            idents_in_scope: VecDeque::new(),
        }
    }

    fn node_gen(&mut self, node: &'a Node) -> Result<GenNode<'a>, Error> {
        // debug only
        self.output += "\n";

        match node {
            Node::FnDeclaration { name: _, body: _ } => self.func_gen(node),
            Node::LetDeclaration { name: _, expr: _ } => self.let_gen(node),
            Node::BinaryExpr {
                left: _,
                right: _,
                operator: _,
            } => self.expr_gen(node),
            _ => {
                return Err(Error::new(
                    ErrorKind::InvalidData,
                    format!("could not generate asm for node {:?}", node),
                ))
            }
        }
    }

    fn let_gen(&mut self, node: &'a Node) -> Result<GenNode<'a>, Error> {
        match node {
            Node::LetDeclaration { name, expr } => {
                // TODO: evaluate actual type size instead of forcing type i64
                self.stack_offset += 8;
                let stack_pos = format!("-{}(%rbp)", self.stack_offset);
                let current_scope = &mut self.idents_in_scope[0];
                current_scope.insert(name.clone(), stack_pos.clone());

                if let Some(expr) = expr {
                    if let Some(reg) = self.expr_gen(expr)?.register {
                        let move_to_stack =
                            format!("movq {}, {}", Self::register_name(reg), stack_pos);
                        self.output += &move_to_stack;
                        self.register_free(reg);
                    }
                }
                return Ok(GenNode {
                    node,
                    register: None,
                });
            }
            _ => {
                return Err(Error::new(
                    ErrorKind::InvalidData,
                    format!("node {:?} is not a variable declaration", node),
                ))
            }
        }
    }

    fn func_gen(&mut self, node: &'a Node) -> Result<GenNode<'a>, Error> {
        match node {
            Node::FnDeclaration { name, body } => {
                self.idents_in_scope.push_front(HashMap::new());
                let fn_label = format!("_{}:\n", name);
                self.output += &fn_label;

                for stmt in body {
                    let _ = self.node_gen(stmt);
                }

                self.idents_in_scope.pop_front();
                return Ok(GenNode {
                    node,
                    register: None,
                });
            }
            _ => {
                return Err(Error::new(
                    ErrorKind::InvalidData,
                    format!("node {:?} is not a function declaration", node),
                ))
            }
        }
    }

    fn find_var_in_scope(&mut self, value: &str) -> Result<String, Error> {
        for scope in &self.idents_in_scope {
            if let Some(address) = scope.get(value) {
                return Ok(address.clone());
            }
        }
        Err(Error::new(
            ErrorKind::InvalidData,
            format!("could not find variable '{}' in current scope.", value),
        ))
    }

    fn expr_gen(&mut self, node: &'a Node) -> Result<GenNode<'a>, Error> {
        match node {
            Node::IntegerLiteral { value } => {
                let register = self.register_alloc()?;
                let move_instruction =
                    format!("movq ${}, {}\n", value, Self::register_name(register));
                self.output += &move_instruction;

                return Ok(GenNode {
                    node,
                    register: Some(register),
                });
            }
            Node::Identifier { value } => {
                let address = self.find_var_in_scope(value)?;
                let register = self.register_alloc()?;
                let move_instruction =
                    format!("movq {}, {}\n", address, Self::register_name(register));
                self.output += &move_instruction;

                return Ok(GenNode {
                    node,
                    register: Some(register),
                });
            }
            Node::BinaryExpr {
                left,
                right,
                operator,
            } => {
                // NOTE: we want to store our result in left_reg because of assignments, where
                // the result of the right node should be moved into the left node, so we flip
                // the left and right registers around in the actual instruction
                let left_reg = option_unwrap!(self.expr_gen(left)?.register, "left_reg is None");
                let right_reg = option_unwrap!(self.expr_gen(right)?.register, "left_reg is None");

                let instruction = format!(
                    "{} {}, {}\n",
                    Self::instruction_name(operator),
                    Self::register_name(right_reg),
                    Self::register_name(left_reg),
                );
                self.output += &instruction;
                self.register_free(right_reg);

                if let Token::Equal = operator {
                    if let Node::Identifier { value } = left.as_ref() {
                        let move_to_stack = format!(
                            "movq {}, {}",
                            Self::register_name(left_reg),
                            self.find_var_in_scope(value)?
                        );
                        self.output += &move_to_stack;
                        self.register_free(left_reg);
                    }
                }

                return Ok(GenNode {
                    node,
                    register: Some(left_reg),
                });
            }
            _ => {
                return Err(Error::new(
                    ErrorKind::InvalidData,
                    format!("node {:?} is not an expression", node),
                ))
            }
        }
    }
}

impl Assembly for LinuxX86Asm {
    fn register_alloc(&mut self) -> Result<Register, Error> {
        for i in 0..REGISTER_SIZE {
            if !self.registers[i] {
                self.registers[i] = true;
                return Ok(i as Register);
            }
        }
        return Err(Error::new(
            ErrorKind::RegisterOverflow,
            "all registers are in use",
        ));
    }

    fn register_free(&mut self, reg: Register) {
        assert!(
            reg < REGISTER_SIZE as Register,
            "reg {} out of bounds. max value is {}",
            reg,
            REGISTER_SIZE
        );

        self.registers[reg as usize] = false;
    }

    fn register_name(reg: Register) -> &'static str {
        match reg {
            0 => "%rbx",
            1 => "%r10",
            2 => "%r11",
            3 => "%r12",
            4 => "%r13",
            5 => "%r14",
            6 => "%r15",
            _ => panic!("reg idex {} out of bounds", reg),
        }
    }

    fn instruction_name(operator: &Token) -> &'static str {
        match operator {
            Token::Plus => "addq",
            Token::Minus => "subq",
            Token::Star => "imul",
            Token::Slash => "idiv",
            Token::Equal => "movq",
            _ => panic!("unexpected token"),
        }
    }

    fn label_create(&mut self) -> usize {
        todo!()
    }

    fn label_name(label: usize) -> &'static str {
        todo!()
    }
}
