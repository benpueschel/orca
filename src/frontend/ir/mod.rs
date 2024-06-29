use std::ops::{Index, IndexMut};

use crate::span::Span;

use super::ast;

pub mod build;
pub mod debug;
pub mod trans;

pub const IR_START_BLOCK: BasicBlock = BasicBlock(0);
pub const IR_END_BLOCK: BasicBlock = BasicBlock(1);
#[derive(Clone, PartialEq)]
pub struct Ir {
    pub basic_blocks: Vec<BasicBlockData>,
    pub scopes: Vec<ScopeData>,
    pub span: Span,
    pub fn_name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarType {
    Usize,
    Unit,
    UserDefined(String),
    Unresolved,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ScopeData {
    pub span: Span,
    pub parent: Option<Scope>,
    pub var_decls: Vec<VarDecl>,
    pub temp_decls: Vec<TempDecl>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Scope(usize);
impl Scope {
    pub fn index(&self) -> usize {
        self.0
    }
}
impl Index<Scope> for Vec<ScopeData> {
    type Output = ScopeData;
    fn index(&self, index: Scope) -> &ScopeData {
        &self[index.index()]
    }
}
impl IndexMut<Scope> for Vec<ScopeData> {
    fn index_mut(&mut self, index: Scope) -> &mut ScopeData {
        &mut self[index.index()]
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct BasicBlock(usize);
impl BasicBlock {
    pub fn index(&self) -> usize {
        self.0
    }
}
impl Index<BasicBlock> for Vec<BasicBlockData> {
    type Output = BasicBlockData;
    fn index(&self, index: BasicBlock) -> &BasicBlockData {
        &self[index.index()]
    }
}
impl IndexMut<BasicBlock> for Vec<BasicBlockData> {
    fn index_mut(&mut self, index: BasicBlock) -> &mut BasicBlockData {
        &mut self[index.index()]
    }
}

#[derive(Clone, PartialEq)]
pub struct BasicBlockData {
    pub statements: Vec<Statement>,
    pub terminator: Option<Terminator>,
    pub scope: Scope,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Terminator {
    pub span: Span,
    pub scope: Scope,
    pub kind: TerminatorKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TerminatorKind {
    Return {
        expr: Operand,
    },
    Goto {
        target: BasicBlock,
    },
    If {
        condition: Rvalue,
        targets: (BasicBlock, BasicBlock),
    },
}

#[derive(Clone, PartialEq)]
pub struct Statement {
    pub span: Span,
    pub kind: StatementKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementKind {
    Assign(Lvalue, Operand),
    Modify(Lvalue, ExprOperator, Operand),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lvalue {
    Variable(Var),
    Temp(TempVal),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Rvalue {
    IntegerLit(usize),
    Variable(Var),
    Temp(TempVal),
    BinaryExpr(ExprOperator, Operand, Operand),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprOperator {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Gt,
    Lt,
    Gte,
    Lte,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    Consume(Lvalue),
    IntegerLit(usize),
    Unit,
}

impl From<Operand> for Rvalue {
    fn from(operand: Operand) -> Rvalue {
        match operand {
            Operand::Consume(lvalue) => lvalue.into(),
            Operand::IntegerLit(value) => Rvalue::IntegerLit(value),
            Operand::Unit => panic!("unit operand is not a valid rvalue"),
        }
    }
}

impl From<Lvalue> for Rvalue {
    fn from(lvalue: Lvalue) -> Rvalue {
        match lvalue {
            Lvalue::Variable(var) => Rvalue::Variable(var),
            Lvalue::Temp(temp) => Rvalue::Temp(temp),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TempVal(usize);
impl TempVal {
    pub fn value(&self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TempDecl {
    pub scope: Scope,
}

const VAR_UNINITIALIZED: usize = usize::MAX;
#[derive(Debug, Clone, PartialEq)]
pub struct Var {
    pub name: String,
    pub id: usize,
    pub var_type: ast::Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
    pub var: Var,
    pub scope: Scope,
}

impl Ir {
    pub fn basic_block_data(&self, block: BasicBlock) -> &BasicBlockData {
        &self.basic_blocks[block.index()]
    }
    pub fn basic_block_data_mut(&mut self, block: BasicBlock) -> &mut BasicBlockData {
        &mut self.basic_blocks[block.index()]
    }
    pub fn scope_data(&self, scope: Scope) -> &ScopeData {
        &self.scopes[scope.index()]
    }
    pub fn scope_data_mut(&mut self, scope: Scope) -> &mut ScopeData {
        &mut self.scopes[scope.index()]
    }
    pub fn find_declaration(&mut self, var: &Var) -> &mut VarDecl {
        // let id = scope_index.0 << 32 | i;
        let scope_index = var.id >> 32;
        let var_index = var.id & 0xFFFF_FFFF;

        println!("id: {:x}", var.id);
        println!("scope_index: {:x}, var_index: {:x}", scope_index, var_index);

        let scope = &mut self.scopes[scope_index];
        &mut scope.var_decls[var_index]
    }
}
