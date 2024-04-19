use std::collections::HashMap;

use crate::frontend::ir::{
    BasicBlock, Ir, Lvalue, Operand, Rvalue, StatementKind, TempVal, TerminatorKind, Var,
    IR_START_BLOCK,
};

pub const REGISTER_SIZE: Register = 7;
pub const ALL_REGISTERS_ALLOCATED: Register = 2 * (1 << (REGISTER_SIZE - 1)) - 1;

#[allow(clippy::identity_op)]
pub const RAX: Register = 1 << (REGISTER_SIZE + 0);
pub const RCX: Register = 1 << (REGISTER_SIZE + 1);
pub const RDX: Register = 1 << (REGISTER_SIZE + 2);
pub const RDI: Register = 1 << (REGISTER_SIZE + 3);
pub const RSI: Register = 1 << (REGISTER_SIZE + 4);
pub const RBP: Register = 1 << (REGISTER_SIZE + 5);
pub const RSP: Register = 1 << (REGISTER_SIZE + 6);

pub type Register = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RegisterNode(usize);
impl RegisterNode {
    pub fn value(&self) -> usize {
        self.0
    }
    pub fn new(value: usize) -> Self {
        RegisterNode(value)
    }
}
impl From<Var> for RegisterNode {
    fn from(var: Var) -> Self {
        RegisterNode(var.id)
    }
}
impl From<TempVal> for RegisterNode {
    fn from(temp: TempVal) -> Self {
        RegisterNode(temp.value())
    }
}

pub struct RegisterGraph {
    nodes: HashMap<RegisterNode, RegisterNodeData>,
    scratch: ScratchRegisters,
    stack_offset: i64,
}

pub struct RegisterNodeData {
    pub edges: Vec<RegisterNode>,
    pub location: ScratchLocation,
}

pub enum ScratchLocation {
    Register(Register),
    Stack(i64),
    /// This should not be used outside of the register allocator.
    /// It indicates a node that has not been processed yet. If you encounter this outside of the
    /// register allocator, it is a bug.
    Unassigned,
}

impl Default for RegisterGraph {
    fn default() -> Self {
        RegisterGraph {
            nodes: HashMap::new(),
            scratch: ScratchRegisters::new(),
            stack_offset: 0,
        }
    }
}

impl RegisterGraph {
    #[allow(unused)]
    pub fn new() -> Self {
        RegisterGraph::default()
    }
    pub fn build_graph(&mut self, ir: &Ir) {
        self.traverse_graph(ir, IR_START_BLOCK);
    }

    fn traverse_graph(&mut self, ir: &Ir, block: BasicBlock) {
        let block = ir.basic_block_data(block);
        for i in 0..block.statements.len() {
            let stmt = &block.statements[i];
            match &stmt.kind {
                StatementKind::Assign(lhs, operand) => {
                    self.process_lvalue(lhs);
                    self.process_operand(operand);
                }
                StatementKind::Modify(lhs, _, operand) => {
                    self.process_lvalue(lhs);
                    self.process_operand(operand);
                }
            }
        }
        if let Some(terminator) = &block.terminator {
            match &terminator.kind {
                TerminatorKind::Goto { target } => {
                    self.traverse_graph(ir, *target);
                }
                TerminatorKind::If { condition, targets } => {
                    self.process_rvalue(condition);
                    self.traverse_graph(ir, targets.0);
                    self.traverse_graph(ir, targets.1);
                }
                TerminatorKind::Return { expr } => {
                    self.process_operand(expr);
                }
            }
        }
    }

    fn process_lvalue(&mut self, lvalue: &Lvalue) {
        let index = match lvalue {
            Lvalue::Variable(var) => {
                if self.nodes.contains_key(&var.clone().into()) {
                    return;
                }
                self.stack_offset -= 8;
                self.nodes.insert(var.clone().into(), RegisterNodeData {
                    edges: Vec::new(),
                    location: ScratchLocation::Stack(self.stack_offset),
                });
                return;
            },
            Lvalue::Temp(temp) => temp.value(),
        };
        let _node = self.get_node_or_insert(RegisterNode(index));
    }

    fn process_rvalue(&mut self, rvalue: &Rvalue) {
        let index = match rvalue {
            Rvalue::Variable(var) => {
                if self.nodes.contains_key(&var.clone().into()) {
                    return;
                }
                self.stack_offset -= 8;
                self.nodes.insert(var.clone().into(), RegisterNodeData {
                    edges: Vec::new(),
                    location: ScratchLocation::Stack(self.stack_offset),
                });
                return;
            }
            Rvalue::Temp(temp) => temp.value(),
            Rvalue::BinaryExpr(_, lhs, rhs) => {
                self.process_operand(lhs);
                self.process_operand(rhs);
                return;
            }
            Rvalue::IntegerLit(_) => return,
        };
        let _node = self.get_node_or_insert(RegisterNode(index));
    }

    fn process_operand(&mut self, operand: &Operand) {
        match operand {
            Operand::Consume(lvalue) => self.process_lvalue(lvalue),
            Operand::Unit => {}
            Operand::IntegerLit(_) => {}
        }
    }

    fn get_node_or_insert(&mut self, node: RegisterNode) -> &mut RegisterNodeData {
        self.nodes.entry(node).or_insert_with(|| {
            let location = self
                .scratch
                .allocate()
                .map(ScratchLocation::Register)
                .unwrap_or_else(|| {
                    self.stack_offset -= 8;
                    ScratchLocation::Stack(self.stack_offset)
                });
            RegisterNodeData {
                edges: Vec::new(),
                location,
            }
        })
    }

    pub fn node_data(&self, node: RegisterNode) -> Option<&RegisterNodeData> {
        self.nodes.get(&node)
    }
    pub fn node_data_mut(&mut self, node: RegisterNode) -> Option<&mut RegisterNodeData> {
        self.nodes.get_mut(&node)
    }
}

#[derive(Default)]
pub struct ScratchRegisters {
    registers: Register,
}

impl ScratchRegisters {
    pub fn new() -> Self {
        ScratchRegisters::default()
    }

    pub fn allocate(&mut self) -> Option<Register> {
        if self.registers ^ ALL_REGISTERS_ALLOCATED == 0 {
            return None;
        }
        let mut current_reg = 1 << 0;
        loop {
            if self.registers & current_reg == 0 {
                self.registers |= current_reg;
                return Some(current_reg);
            }
            current_reg <<= 1;
        }
    }

    pub fn free(&mut self, reg: Register) {
        self.registers ^= reg;
    }

    pub fn get_name(reg: Register, size: usize) -> &'static str {
        match size {
            8 => match reg {
                1 => "%rbx",
                2 => "%r10",
                4 => "%r11",
                8 => "%r12",
                16 => "%r13",
                32 => "%r14",
                64 => "%r15",
                RAX => "%rax",
                RCX => "%rcx",
                RDX => "%rdx",
                RDI => "%rdi",
                RSI => "%rsi",
                RBP => "%rbp",
                RSP => "%rsp",
                x => panic!("{} is not a valid register", x),
            },
            4 => match reg {
                1 => "%ebx",
                2 => "%r10d",
                4 => "%r11d",
                8 => "%r12d",
                16 => "%r13d",
                32 => "%r14d",
                64 => "%r15d",
                RAX => "%eax",
                RCX => "%ecx",
                RDX => "%edx",
                RDI => "%edi",
                RSI => "%esi",
                RBP => "%ebp",
                RSP => "%esp",
                x => panic!("{} is not a valid register", x),
            },
            2 => match reg {
                1 => "%bx",
                2 => "%r10w",
                4 => "%r11w",
                8 => "%r12w",
                16 => "%r13w",
                32 => "%r14w",
                64 => "%r15w",
                RAX => "%ax",
                RCX => "%cx",
                RDX => "%dx",
                RDI => "%di",
                RSI => "%si",
                RBP => "%bp",
                RSP => "%sp",
                x => panic!("{} is not a valid register", x),
            },
            1 => match reg {
                1 => "%bl",
                2 => "%r10l",
                4 => "%r11l",
                8 => "%r12l",
                16 => "%r13l",
                32 => "%r14l",
                64 => "%r15l",
                RAX => "%al",
                RCX => "%cl",
                RDX => "%dl",
                RDI => "%dil",
                RSI => "%sil",
                RBP => "%bpl",
                RSP => "%spl",
                x => panic!("{} is not a valid register", x),
            },
            _ => panic!("register size {} is not supported.", size),
        }
    }
}
