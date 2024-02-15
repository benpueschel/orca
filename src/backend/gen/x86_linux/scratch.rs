use super::Graph;

pub const REGISTER_SIZE: Register = 7;
pub const ALL_REGISTERS_ALLOCATED: Register = 2 * (1 << (REGISTER_SIZE - 1)) - 1;

pub const RAX: Register = 1 << REGISTER_SIZE + 0;
pub const RCX: Register = 1 << REGISTER_SIZE + 1;
pub const RDX: Register = 1 << REGISTER_SIZE + 2;
pub const RDI: Register = 1 << REGISTER_SIZE + 3;
pub const RSI: Register = 1 << REGISTER_SIZE + 4;
pub const RBP: Register = 1 << REGISTER_SIZE + 5;
pub const RSP: Register = 1 << REGISTER_SIZE + 6;

pub type Register = usize;

struct RegisterGraph {
    nodes: Vec<RegisterNode>,
}

type RegisterNodeIndex = usize;
struct RegisterNode {
    edges: Vec<RegisterNodeIndex>,
    register: Register,
}

pub struct ScratchRegisters {
    registers: Register,
}

impl ScratchRegisters {
    pub fn new() -> Self {
        ScratchRegisters {
            registers: 0
        }
    }

    pub fn allocate(&mut self) -> Option<Register> {
        if self.registers ^ ALL_REGISTERS_ALLOCATED == 0 {
            return None;
        }
        let mut current_reg = 1 << 0;
        loop {
            if self.registers ^ current_reg != 0 {
                return Some(current_reg);
            }
            current_reg <<= 1;
        }
    }

    pub fn free(&mut self, reg: Register) {
        if self.registers & reg != 0 {
            self.registers ^= reg;
        }
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
