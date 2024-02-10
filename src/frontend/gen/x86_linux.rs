use std::{
    collections::{HashMap, VecDeque},
    ops::AddAssign,
};

use crate::{
    error::{Error, ErrorKind},
    option_unwrap,
};
use crate::frontend::{
    ast::{BinaryExprData, FnDeclData, Node, ProgramData, ReturnData},
    gen::Assembly,
    lexer::Token,
};

trait StringAppend {
    fn append(&mut self, str: String);
}

impl StringAppend for String {
    fn append(&mut self, str: String) {
        self.add_assign(&str);
    }
}

use super::{CodeGenerator, GenNode, Register};

const REGISTER_SIZE: usize = 6;

pub struct LinuxX86Asm {
    registers: [bool; REGISTER_SIZE],
    current_label: usize,
    output: String,
    stack_offset: usize,
    idents_in_scope: VecDeque<HashMap<String, String>>,
}

impl<'a> CodeGenerator<'a> for LinuxX86Asm {
    fn generate_assembly(&mut self, node: &'a Node) -> Result<&str, Error> {
        if let Node::Program(ProgramData { body }) = node {
            self.idents_in_scope.push_front(HashMap::new());
            for node in body {
                let _ = self.node_gen(node);
            }
            self.idents_in_scope.pop_back();
            return Ok(&self.output);
        }
        Err(Error::new(ErrorKind::InvalidData, "node is not a program."))
    }
}

impl<'a> LinuxX86Asm {
    pub fn new() -> Self {
        LinuxX86Asm {
            registers: [false; REGISTER_SIZE],
            current_label: 0,
            output: String::new(),
            stack_offset: 0,
            idents_in_scope: VecDeque::new(),
        }
    }

    fn node_gen(&mut self, node: &'a Node) -> Result<GenNode<'a>, Error> {
        // debug only
        self.output += "\n";

        match node {
            Node::FnDeclaration(_) => self.func_gen(node),
            Node::IfStatement(_) => self.if_gen(node),
            Node::ReturnStatement(_) => self.return_gen(node),
            Node::LetDeclaration(_) => self.let_gen(node),
            Node::BinaryExpr(_) => self.expr_gen(node),
            _ => {
                return Err(Error::new(
                    ErrorKind::InvalidData,
                    format!("could not generate asm for node {:?}", node),
                ))
            }
        }
    }

    fn if_gen(&mut self, node: &'a Node) -> Result<GenNode<'a>, Error> {
        if let Node::IfStatement(data) = node {
            let label = Self::label_name(self.label_create());
            let reg = self
                .expr_gen(&data.expr)?
                .register
                .expect("could not allocate register");

            self.output
                .append(format!("cmp $0, {}\n", Self::register_name(reg)));
            self.register_free(reg);

            if data.else_body.len() > 0 {
                self.output.append(format!("je {}_ELSE\n", label));
            }
            for statement in &data.body {
                self.node_gen(&statement)?;
            }
            self.output.append(format!("jmp {}_DONE\n", label));

            self.output.append(format!("{}_ELSE:\n", label));
            for statement in &data.else_body {
                self.node_gen(&statement)?;
            }

            self.output.append(format!("{}_DONE:\n", label));
            return Ok(GenNode {
                node,
                register: None,
            });
        }

        Err(Error::new(
            ErrorKind::InvalidData,
            format!("node {:?} is not an if statement.", node),
        ))
    }

    fn return_gen(&mut self, node: &'a Node) -> Result<GenNode<'a>, Error> {
        if let Node::ReturnStatement(ReturnData { expr }) = node {
            if let Some(x) = expr {
                if let Some(reg) = self.expr_gen(x)?.register {
                    let instruction = format!("movq {}, %rax\n", Self::register_name(reg));
                    self.output += &instruction;
                }
            }
            // TODO: rethink design decision. should %rbp
            // be popped in the return statement? will
            // definitely lead to problems with scoped blocks
            self.output += "popq %rbp\n";
            self.output += "retq\n";
            return Ok(GenNode {
                node,
                register: None,
            });
        }
        Err(Error::new(
            ErrorKind::InvalidData,
            format!("node {:?} is not a return statement", node),
        ))
    }

    fn let_gen(&mut self, node: &'a Node) -> Result<GenNode<'a>, Error> {
        if let Node::LetDeclaration(data) = node {
            // TODO: evaluate actual type size instead of forcing type i64
            self.stack_offset += 8;
            let stack_pos = format!("-{}(%rbp)", self.stack_offset);
            let current_scope = &mut self.idents_in_scope[0];
            current_scope.insert(data.name.clone(), stack_pos.clone());

            if let Some(expr) = &data.expr {
                if let Some(reg) = self.expr_gen(expr)?.register {
                    let move_to_stack =
                        format!("movq {}, {}\n", Self::register_name(reg), stack_pos);
                    self.output += &move_to_stack;
                    self.register_free(reg);
                }
            }
            return Ok(GenNode {
                node,
                register: None,
            });
        }
        Err(Error::new(
            ErrorKind::InvalidData,
            format!("node {:?} is not a variable declaration", node),
        ))
    }

    fn func_gen(&mut self, node: &'a Node) -> Result<GenNode<'a>, Error> {
        if let Node::FnDeclaration(FnDeclData { name, body, return_type: _ }) = node {
            self.idents_in_scope.push_front(HashMap::new());
            let fn_label = format!("{}", name);
            self.output.append(format!(".globl {}\n", &fn_label));
            self.output.append(format!("{}:\n", &fn_label));

            self.output += "pushq %rbp\n"; // set up new stack frame
            self.output += "movq %rsp, %rbp\n";
            self.output += "movl $0, -4(%rbp)\n";
            self.stack_offset += 4;

            for stmt in body {
                let _ = self.node_gen(stmt);
            }

            // self.output += "popq %rbp\n";
            self.idents_in_scope.pop_front();
            return Ok(GenNode {
                node,
                register: None,
            });
        }
        Err(Error::new(
            ErrorKind::InvalidData,
            format!("node {:?} is not a function declaration", node),
        ))
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
            Node::IntegerLiteral(value) => {
                let register = self.register_alloc()?;
                let move_instruction =
                    format!("movq ${}, {}\n", value, Self::register_name(register));
                self.output += &move_instruction;

                return Ok(GenNode {
                    node,
                    register: Some(register),
                });
            }
            Node::Identifier(value) => {
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
            Node::BinaryExpr(BinaryExprData {
                left,
                right,
                operator,
            }) => {
                // NOTE: we want to store our result in left_reg because of assignments, where
                // the result of the right node should be moved into the left node, so we flip
                // the left and right registers around in the actual instruction
                let left_reg = option_unwrap!(self.expr_gen(left)?.register, "left_reg is None");
                let right_reg = option_unwrap!(self.expr_gen(right)?.register, "right_reg is None");

                let instruction = format!(
                    "{} {}, {}\n",
                    Self::instruction_name(operator),
                    Self::register_name(right_reg),
                    Self::register_name(left_reg),
                );
                self.output += &instruction;
                self.register_free(right_reg);

                if let Token::Equal = operator {
                    if let Node::Identifier(value) = left.as_ref() {
                        let move_to_stack = format!(
                            "movq {}, {}\n",
                            Self::register_name(left_reg),
                            self.find_var_in_scope(value)?
                        );
                        self.output += &move_to_stack;
                        self.register_free(left_reg);
                    }
                }

                // comparison
                match operator {
                    Token::LeftCaret => {
                        self.output += "setl %al\n";
                        self.output += "andb $1, %al\n";
                    }
                    Token::RightCaret => {
                        self.output += "setg %al\n";
                        self.output += "andb $1, %al\n";
                    }
                    _ => {}
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
            Token::LeftCaret => "cmpq",
            Token::RightCaret => "cmpq",
            Token::Plus => "addq",
            Token::Minus => "subq",
            Token::Star => "imul",
            Token::Slash => "idiv",
            Token::Equal => "movq",
            _ => panic!("unexpected token"),
        }
    }

    fn label_create(&mut self) -> usize {
        self.current_label += 1;
        self.current_label
    }

    fn label_name(label: usize) -> String {
        format!(".L{}", label)
    }
}
