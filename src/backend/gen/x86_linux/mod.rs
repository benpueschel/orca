use std::collections::{HashMap, VecDeque};

use crate::frontend::{
    ast::{BinaryExprData, FnDeclData, IfData, LetDeclData, Node, NodeType, ReturnData, Type},
    lexer::{Token, TokenType},
};

use self::{
    assembly_node::{AssemblyNode, Expression, Instruction, JumpCondition},
    scratch::{Register, ScratchRegisters},
};

use super::{CodeGenerator, Variable};

pub mod assembly_node;
pub mod codegen;
pub mod scratch;

type GraphNodeIndex = usize;

#[derive(Debug, Clone, PartialEq)]
pub struct Graph {
    nodes: Vec<GraphNode>,
    pub start_node: Option<GraphNodeIndex>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GraphNode {
    nodes: Vec<AssemblyNode>,
    register: Option<Register>,
    branches: Vec<GraphNodeIndex>,
}

pub struct X86Linux {
    registers: ScratchRegisters,
    pub nodes: Vec<AssemblyNode>,
    vars_in_scope: Vec<HashMap<String, Variable>>,

    stack_pointer: usize,
    label_counter: usize,
}

impl CodeGenerator for X86Linux {
    fn new(node: Node) -> Self {
        let mut gen = X86Linux {
            registers: ScratchRegisters::new(),
            nodes: vec![],
            stack_pointer: 0,
            label_counter: 0,
            vars_in_scope: vec![],
        };
        let mut nodes = gen.gen_node(node);
        gen.nodes.append(&mut nodes);
        gen
    }
}

impl X86Linux {
    fn process_nodes(&mut self, nodes: &Vec<Node>) -> Vec<AssemblyNode> {
        let mut nodes = VecDeque::from(nodes.clone());
        let mut assembly_nodes = vec![];

        while !nodes.is_empty() {
            let node = nodes.pop_front().unwrap();
            assembly_nodes.append(&mut self.gen_node(node));
        }

        assembly_nodes
    }

    fn gen_node(&mut self, node: Node) -> Vec<AssemblyNode> {
        match node.node_type {
            NodeType::FnDeclaration(data) => self.gen_func(data),
            NodeType::LetDeclaration(data) => self.gen_let(data),
            NodeType::BinaryExpr(data) => self.gen_expression(data),
            NodeType::IfStatement(data) => self.gen_if(data),
            NodeType::ReturnStatement(data) => self.gen_return(data),
            NodeType::Program(data) => self.process_nodes(&data.body),
            x => panic!("could not process {:?}", x),
        }
    }

    fn gen_return(&mut self, data: ReturnData) -> Vec<AssemblyNode> {
        let mut instructions = vec![];

        let return_value = match data.expr {
            Some(expr) => match expr.node_type {
                NodeType::BinaryExpr(data) => {
                    instructions.append(&mut self.gen_expression(data));
                    instructions
                        .last()
                        .expect("could not extract return value")
                        .right
                        .clone()
                }
                NodeType::Identifier(x) => Expression::Memory(x.name),
                NodeType::IntegerLiteral(x) => Expression::IntegerLiteral(x),
                x => panic!("{:?} is not a valid expression", x),
            },
            None => Expression::None,
        };

        instructions.append(&mut vec![
            AssemblyNode {
                instruction: Instruction::Mov,
                left: return_value,
                right: Expression::Register(scratch::RAX),
                size: 8, // TODO: dynamic types
            },
            AssemblyNode {
                instruction: Instruction::Jmp(JumpCondition::None),
                left: Expression::Label(format!(".{}.cleanup", data.fn_name)),
                right: Expression::None,
                size: 0,
            },
        ]);
        instructions
    }

    fn gen_func(&mut self, data: FnDeclData) -> Vec<AssemblyNode> {
        self.vars_in_scope.push(HashMap::new());

        let mut instructions = vec![
            AssemblyNode {
                instruction: Instruction::AssemblyDirective,
                left: Expression::Label(format!(".globl {}", data.name)),
                right: Expression::None,
                size: 0,
            },
            // function label (.NAME:)
            AssemblyNode {
                instruction: Instruction::LabelDeclaration,
                left: Expression::Label(data.name.clone()),
                right: Expression::None,
                size: 0,
            },
            // push rbp onto the stack (pushq %rbp)
            AssemblyNode {
                instruction: Instruction::Push,
                left: Expression::Register(scratch::RBP),
                right: Expression::None,
                size: 8,
            },
            // move rsp into rbp (movq %rsp, %rbp)
            AssemblyNode {
                instruction: Instruction::Mov,
                left: Expression::Register(scratch::RSP),
                right: Expression::Register(scratch::RBP),
                size: 8,
            },
        ];

        instructions.append(&mut self.process_nodes(&data.body));

        instructions.append(&mut vec![
            // function cleanup label (.NAME.cleanup:)
            AssemblyNode {
                instruction: Instruction::LabelDeclaration,
                left: Expression::Label(format!(".{}.cleanup", data.name)),
                right: Expression::None,
                size: 0,
            },
            // pop previously saved stack pointer from the stack
            AssemblyNode {
                instruction: Instruction::Pop,
                left: Expression::Register(scratch::RBP),
                right: Expression::None,
                size: 8,
            },
            // return
            AssemblyNode {
                instruction: Instruction::Ret,
                left: Expression::None,
                right: Expression::None,
                size: 0,
            },
        ]);

        self.vars_in_scope.pop();

        instructions
    }

    fn gen_if(&mut self, data: IfData) -> Vec<AssemblyNode> {
        self.vars_in_scope.push(HashMap::new());

        let else_label = self.label_alloc();
        let done_label = self.label_alloc();
        let mut instructions = vec![];

        let mut jump_condition = JumpCondition::None;
        if let NodeType::BinaryExpr(expr) = &data.expr.node_type {
            jump_condition = match &expr.operator.token_type {
                //TODO: we need to create a type "ComparisonOperator" to handle this (== vs =)
                // TokenType::Equal => JumpCondition::NotEqual,
                TokenType::LeftCaret => JumpCondition::GreaterOrEqual,
                TokenType::RightCaret => JumpCondition::LessOrEqual,
                x => panic!("{:?} is not a valid comparison operator", x),
            };
        }

        // TODO: compare expression to 0 if it's not a comparison
        instructions.append(&mut self.gen_node(*data.expr));
        instructions.push(AssemblyNode {
            instruction: Instruction::Jmp(jump_condition),
            left: else_label.clone(),
            right: Expression::None,
            size: 0,
        });

        instructions.append(&mut self.process_nodes(&data.body));
        instructions.append(&mut vec![
            AssemblyNode {
                instruction: Instruction::Jmp(JumpCondition::None),
                left: done_label.clone(),
                right: Expression::None,
                size: 0,
            },
            AssemblyNode {
                instruction: Instruction::LabelDeclaration,
                left: else_label.clone(),
                right: Expression::None,
                size: 0,
            },
        ]);
        instructions.append(&mut self.process_nodes(&data.else_body));
        instructions.push(AssemblyNode {
            instruction: Instruction::LabelDeclaration,
            left: done_label.clone(),
            right: Expression::None,
            size: 0,
        });

        self.vars_in_scope.pop();
        instructions
    }

    fn gen_let(&mut self, data: LetDeclData) -> Vec<AssemblyNode> {
        let mut instructions = vec![];
        let stack_pos = self.stack_alloc(Type::Usize); // TODO: dynamic types
        self.scope_push(data.name.clone(), stack_pos.clone(), Type::Usize); // TODO: dynamic types
        if let Some(expr) = data.expr {
            let expr_result = match expr.node_type {
                NodeType::IntegerLiteral(x) => Expression::IntegerLiteral(x),
                NodeType::Identifier(x) => self.find_var(x.name).current_location.clone(),
                NodeType::BinaryExpr(x) => {
                    instructions.append(&mut self.gen_expression(x));
                    instructions.last().unwrap().clone_result()
                }
                x => panic!("{:?} is not a valid expression", x),
            };
            instructions.push(AssemblyNode {
                instruction: Instruction::Mov,
                left: expr_result,
                right: stack_pos,
                size: 8, // TODO: dynamic types
            });
        }
        instructions
    }

    fn gen_expression(&mut self, data: BinaryExprData) -> Vec<AssemblyNode> {
        let mut nodes = vec![];

        // NOTE: we swap left and right because x86 operations are right-hand destructive in AT&T syntax
        let mut right = match data.left.node_type {
            NodeType::IntegerLiteral(x) => Expression::IntegerLiteral(x),
            NodeType::Identifier(x) => self.find_var(x.name).current_location.clone(),
            NodeType::BinaryExpr(x) => {
                nodes.append(&mut self.gen_expression(x));
                nodes.last().unwrap().clone_result()
            }
            _ => panic!(""),
        };
        let left = match data.right.node_type {
            NodeType::IntegerLiteral(x) => Expression::IntegerLiteral(x),
            NodeType::Identifier(x) => self.find_var(x.name).current_location.clone(),
            NodeType::BinaryExpr(x) => {
                nodes.append(&mut self.gen_expression(x));
                nodes.last().unwrap().clone_result()
            }
            _ => panic!(""),
        };

        if let TokenType::Equal = data.operator.token_type {
            // TODO: something ig
        } else {
            match right {
                Expression::Register(_) => {}
                _ => {
                    let temp_reigster = self.registers.allocate().expect("register overflow");
                    nodes.push(AssemblyNode {
                        instruction: Instruction::Mov,
                        left: right.clone(),
                        right: Expression::Register(temp_reigster),
                        size: 8, // TODO: dynamic types
                    });
                    right = Expression::Register(temp_reigster);
                    self.registers.free(temp_reigster);
                }
            };
        }

        let instruction = Self::get_instruction(data.operator);
        nodes.push(AssemblyNode {
            instruction,
            left,
            right,
            size: 8, // TODO: dynamic types
        });

        nodes
    }

    fn find_var(&self, name: String) -> Variable {
        for scope in self.vars_in_scope.iter().rev() {
            if let Some(var) = scope.get(&name) {
                return var.clone();
            }
        }
        panic!("variable {} not found", name);
    }

    fn scope_push(&mut self, identifier: String, expression: Expression, r#type: Type) {
        self.vars_in_scope.last_mut().unwrap().insert(
            identifier.clone(),
            Variable {
                identifier,
                r#type,
                current_location: expression,
                memory_location: None,
            },
        );
    }

    fn label_alloc(&mut self) -> Expression {
        let label = format!(".L{}", self.label_counter);
        self.label_counter += 1;
        Expression::Label(label)
    }

    fn stack_alloc(&mut self, r#type: Type) -> Expression {
        let type_size = match r#type {
            Type::Usize => 8,
        };
        self.stack_pointer += type_size;
        Expression::Memory(format!(
            "-{}({})",
            self.stack_pointer,
            ScratchRegisters::get_name(scratch::RBP, type_size)
        ))
    }

    fn get_instruction(operator: Token) -> Instruction {
        match operator.token_type {
            TokenType::Equal => Instruction::Mov,
            TokenType::Plus => Instruction::Add,
            TokenType::Minus => Instruction::Sub,
            TokenType::Star => Instruction::IMul,
            TokenType::Slash => Instruction::IDiv,
            TokenType::LeftCaret => Instruction::Cmp,
            TokenType::RightCaret => Instruction::Cmp,
            x => panic!("{:?} is not an operator", x),
        }
    }
}
