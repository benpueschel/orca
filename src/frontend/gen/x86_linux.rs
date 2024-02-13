use std::{
    collections::{HashMap, VecDeque},
    ops::AddAssign,
};

use crate::frontend::{
    ast::{BinaryExprData, FnDeclData, Node, ProgramData, ReturnData},
    gen::Assembly,
    lexer::Token,
};
use crate::{
    error::{Error, ErrorKind},
    frontend::ast::Type,
    option_unwrap,
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
const TOTAL_REGISTERS: usize = REGISTER_SIZE * 4;

struct Variable {
    r#type: Type,
    mem_position: String,
}

pub struct LinuxX86Asm {
    registers: [bool; TOTAL_REGISTERS],
    current_label: usize,
    output: String,
    stack_offset: usize,
    idents_in_scope: VecDeque<HashMap<String, Variable>>,
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

fn eval_type_size(r#type: Type) -> usize {
    match r#type {
        Type::Usize => 8,
        Type::U32 => 4,
        Type::Identifier(_) => panic!("custom types are not supported"),
    }
}

fn eval_reg_size(reg: Register) -> usize {
    // FIXME: ugly. really ugly. i cannot overstate just how ugly this all is.
    return (2 as usize).pow(3 - reg as u32 / REGISTER_SIZE as u32);
}

fn eval_move_instruction(reg: Register) -> &'static str {
    match eval_reg_size(reg) {
        1 => "movb",
        2 => "movw",
        4 => "movl",
        8 => "movq",
        x => panic!("{} is not a valid register size.", x),
    }
}

impl<'a> LinuxX86Asm {
    pub fn new() -> Self {
        LinuxX86Asm {
            registers: [false; TOTAL_REGISTERS],
            current_label: 0,
            output: String::new(),
            stack_offset: 0,
            idents_in_scope: VecDeque::new(),
        }
    }

    fn node_gen(&'a mut self, node: &'a Node) -> Result<GenNode<'a>, Error> {
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
                    let return_reg = match eval_reg_size(reg) {
                        8 => "%rax",
                        4 => "%eax",
                        2 => "%ax",
                        1 => "%al",
                        x => panic!("oh no, reg size invalid. you made groggo sad :("),
                    };
                    let instruction = format!(
                        "{} {}, {}\n",
                        eval_move_instruction(reg),
                        Self::register_name(reg),
                        return_reg
                    );
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
            let r#type = data
                .r#type
                .clone()
                .expect("type inference is not supported yet.");
            self.stack_offset += eval_type_size(r#type.clone());

            let stack_pos = format!("-{}(%rbp)", self.stack_offset);
            let current_scope = &mut self.idents_in_scope[0];
            current_scope.insert(
                data.name.clone(),
                Variable {
                    r#type,
                    mem_position: stack_pos.clone(),
                },
            );

            if let Some(expr) = &data.expr {
                if let Some(reg) = self.expr_gen(expr)?.register {
                    let move_to_stack = format!(
                        "{} {}, {}\n",
                        eval_move_instruction(reg),
                        Self::register_name(reg),
                        stack_pos
                    );
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
        if let Node::FnDeclaration(FnDeclData {
            name,
            body,
            return_type: _,
        }) = node
        {
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

    fn find_var_in_scope(&'a self, value: &str) -> Result<&'a Variable, Error> {
        for scope in &self.idents_in_scope {
            if let Some(address) = scope.get(value) {
                return Ok(&address);
            }
        }
        Err(Error::new(
            ErrorKind::InvalidData,
            format!("could not find variable '{}' in current scope.", value),
        ))
    }

    fn expr_gen(&'a mut self, node: &'a Node) -> Result<GenNode<'a>, Error> {
        match node {
            Node::IntegerLiteral(value) => {
                // FIXME: infer type, else everything crumbles into pieces. this currently does not
                // support u32, unless you specifically set the integer literals to use 32 bit
                // registers, in which case usize doesn't work. so you also can't mix types
                // together (which makes sense because we haven't implemented casting either)
                let register = self.register_alloc(8)?;
                let move_instruction = format!(
                    "{} ${}, {}\n",
                    eval_move_instruction(register),
                    value,
                    Self::register_name(register)
                );
                self.output += &move_instruction;

                return Ok(GenNode {
                    node,
                    register: Some(register),
                });
            }
            Node::Identifier(value) => {
                // FIXME: incredibly ugly, the cloning madness is only
                // done to satisfy the borrow checker
                let var = self.find_var_in_scope(value)?;
                let mem_position = var.mem_position.clone();
                let register = self.register_alloc(eval_type_size(var.r#type.clone()))?;
                let move_instruction = format!(
                    "{} {}, {}\n",
                    eval_move_instruction(register),
                    mem_position,
                    Self::register_name(register)
                );
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
                let mut left_reg =
                    option_unwrap!(self.expr_gen(left)?.register, "left_reg is None");
                let right_reg = option_unwrap!(self.expr_gen(right)?.register, "right_reg is None");

                let instruction = format!(
                    "{} {}, {}\n",
                    Self::instruction_name(operator, eval_reg_size(left_reg)),
                    Self::register_name(right_reg),
                    Self::register_name(left_reg),
                );
                self.output += &instruction;
                self.register_free(right_reg);

                if let Token::Equal = operator {
                    if let Node::Identifier(value) = left.as_ref() {
                        let move_to_stack = format!(
                            "{} {}, {}\n",
                            eval_move_instruction(left_reg),
                            Self::register_name(left_reg),
                            self.find_var_in_scope(value)?.mem_position
                        );
                        self.output += &move_to_stack;
                        self.register_free(left_reg);
                    }
                }

                // comparison
                match operator {
                    Token::LeftCaret => {
                        left_reg = self.register_alloc(1)?;
                        let reg = Self::register_name(left_reg);
                        self.output.append(format!("setl {}\n", reg));
                        self.output.append(format!("andb $1, {}\n", reg));
                    }
                    Token::RightCaret => {
                        left_reg = self.register_alloc(1)?;
                        let reg = Self::register_name(left_reg);
                        self.output.append(format!("setg {}\n", reg));
                        self.output.append(format!("andb $1, {}\n", reg));
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
    fn register_alloc(&mut self, type_size: usize) -> Result<Register, Error> {
        let start = match type_size {
            8 => (REGISTER_SIZE + 1) * 0,
            4 => (REGISTER_SIZE + 1) * 1,
            2 => (REGISTER_SIZE + 1) * 2,
            1 => (REGISTER_SIZE + 1) * 3,
            _ => {
                return Err(Error::new(
                    ErrorKind::InvalidData,
                    format!("{} is not a valid register size", type_size),
                ))
            }
        };
        let end = start + REGISTER_SIZE;
        for i in start..end {
            if !self.registers[i] {
                self.registers[i] = true;
                return Ok(i as Register);
            }
        }
        return Err(Error::new(
            ErrorKind::RegisterOverflow,
            format!("all registers of size {} are in use", type_size),
        ));
    }

    fn register_free(&mut self, reg: Register) {
        assert!(
            reg < TOTAL_REGISTERS as Register,
            "reg {} out of bounds. max value is {}",
            reg,
            TOTAL_REGISTERS,
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

            7 => "%ebx",
            8 => "%r10d",
            9 => "%r11d",
            10 => "%r12d",
            11 => "%r13d",
            12 => "%r14d",
            13 => "%r15d",

            14 => "%bx",
            15 => "%r10w",
            16 => "%r11w",
            17 => "%r12w",
            18 => "%r13w",
            19 => "%r14w",
            20 => "%r15w",

            21 => "%bl",
            22 => "%r10b",
            23 => "%r11b",
            24 => "%r12b",
            25 => "%r13b",
            26 => "%r14b",
            27 => "%r15b",
            _ => panic!("reg idex {} out of bounds", reg),
        }
    }

    fn instruction_name(operator: &Token, type_size: usize) -> &'static str {
        match type_size {
            1 => match operator {
                Token::LeftCaret => "cmpb",
                Token::RightCaret => "cmpb",
                Token::Plus => "addb",
                Token::Minus => "subb",
                Token::Star => "imulb",
                Token::Slash => "idivb",
                Token::Equal => "movb",
                _ => panic!("unexpected token"),
            },
            2 => match operator {
                Token::LeftCaret => "cmpw",
                Token::RightCaret => "cmpw",
                Token::Plus => "addw",
                Token::Minus => "subw",
                Token::Star => "imulw",
                Token::Slash => "idivw",
                Token::Equal => "movw",
                _ => panic!("unexpected token"),
            },
            4 => match operator {
                Token::LeftCaret => "cmpl",
                Token::RightCaret => "cmpl",
                Token::Plus => "addl",
                Token::Minus => "subl",
                Token::Star => "imull",
                Token::Slash => "idivl",
                Token::Equal => "movl",
                _ => panic!("unexpected token"),
            },
            8 => match operator {
                Token::LeftCaret => "cmpq",
                Token::RightCaret => "cmpq",
                Token::Plus => "addq",
                Token::Minus => "subq",
                Token::Star => "imulq",
                Token::Slash => "idivq",
                Token::Equal => "movq",
                _ => panic!("unexpected token"),
            },
            x => panic!("{} is not a valid register size.", x),
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
