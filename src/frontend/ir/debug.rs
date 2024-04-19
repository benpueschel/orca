use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Write;

use crate::frontend::ir::IR_END_BLOCK;

use super::ExprOperator;
use super::Lvalue;
use super::Operand;
use super::Rvalue;
use super::StatementKind;
use super::{BasicBlockData, Ir, Statement, Terminator, TerminatorKind, IR_START_BLOCK};

impl Display for Ir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let start = self.basic_block_data(IR_START_BLOCK);
        let debug = start.display_fmt(self, f.alternate())?;
        write!(f, "{}", debug)
    }
}

impl Debug for Ir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let start = self.basic_block_data(IR_START_BLOCK);
        let debug = start.debug_fmt(self, f.alternate())?;
        f.debug_tuple("Ir").field(&debug).finish()
    }
}

impl BasicBlockData {
    pub fn display_fmt(&self, ir: &Ir, pretty: bool) -> Result<DebugString, std::fmt::Error> {
        let mut fmt = DebugString::default();
        if ir.basic_blocks[IR_END_BLOCK] == *self {
            write!(fmt, "end")?;
            return Ok(fmt);
        }

        for statement in &self.statements {
            writeln!(fmt, "{}", statement)?;
        }
        if let Some(terminator) = &self.terminator {
            writeln!(fmt, "{}", terminator.display_fmt(ir, pretty)?)?;
        }
        Ok(fmt)
    }

    pub fn debug_fmt(&self, ir: &Ir, pretty: bool) -> Result<DebugString, std::fmt::Error> {
        let mut fmt = DebugString::default();

        if ir.basic_blocks[IR_END_BLOCK] == *self {
            write!(fmt, "end")?;
            return Ok(fmt);
        }

        #[derive(Debug)]
        #[allow(dead_code)]
        struct Bb {
            stmts: Vec<Statement>,
            term: Option<DebugString>,
        }
        let bb = Bb {
            stmts: self.statements.clone(),
            term: match &self.terminator {
                Some(x) => Some(x.debug_fmt(ir, pretty)?),
                None => None,
            },
        };
        if pretty {
            write!(fmt, "{:#?}", bb)?;
        } else {
            write!(fmt, "{:?}", bb)?;
        }
        Ok(fmt)
    }
}

impl Terminator {
    pub fn debug_fmt(&self, ir: &Ir, pretty: bool) -> Result<DebugString, std::fmt::Error> {
        let mut fmt = DebugString::default();
        let kind = self.kind.debug_fmt(ir, pretty)?;
        if pretty {
            write!(fmt, "{:#?}", kind)?;
        } else {
            write!(fmt, "{:?}", kind)?;
        }
        Ok(fmt)
    }
    pub fn display_fmt(&self, ir: &Ir, pretty: bool) -> Result<DebugString, std::fmt::Error> {
        let mut fmt = DebugString::default();
        let kind = self.kind.display_fmt(ir, pretty)?;
        write!(fmt, "{}", kind)?;
        Ok(fmt)
    }
}

impl TerminatorKind {
    pub fn debug_fmt(&self, ir: &Ir, pretty: bool) -> Result<DebugString, std::fmt::Error> {
        let mut fmt = DebugString::default();
        match self {
            TerminatorKind::Goto { target } => {
                let block = ir.basic_block_data(*target).debug_fmt(ir, pretty)?;
                if pretty {
                    write!(fmt, "goto({:#?})", block)?;
                } else {
                    write!(fmt, "goto({:?})", block)?;
                }
            }
            TerminatorKind::If { condition, targets } => {
                let then_block = ir.basic_block_data(targets.0).debug_fmt(ir, pretty)?;
                let else_block = ir.basic_block_data(targets.1).debug_fmt(ir, pretty)?;
                if pretty {
                    write!(
                        fmt,
                        "if({:#?}, {:#?}, {:#?})",
                        condition, then_block, else_block
                    )?;
                } else {
                    write!(
                        fmt,
                        "if({:?}, {:?}, {:?})",
                        condition, then_block, else_block
                    )?;
                }
            }
            TerminatorKind::Return { expr } => {
                if pretty {
                    write!(fmt, "return {:#?}", expr)?;
                } else {
                    write!(fmt, "return {:?}", expr)?;
                }
            }
        }
        Ok(fmt)
    }
    pub fn display_fmt(&self, ir: &Ir, pretty: bool) -> Result<DebugString, std::fmt::Error> {
        let mut fmt = DebugString::default();
        match self {
            TerminatorKind::Goto { target } => {
                let block = ir.basic_block_data(*target).display_fmt(ir, pretty)?;
                write!(fmt, "goto:{{\n{}}}", block)?;
            }
            TerminatorKind::If { condition, targets } => {
                let then_block = ir.basic_block_data(targets.0).display_fmt(ir, pretty)?;
                let else_block = ir.basic_block_data(targets.1).display_fmt(ir, pretty)?;
                write!(
                    fmt,
                    "if({}){{\n{}\n}}else{{:\n{}}})",
                    condition, then_block, else_block
                )?;
            }
            TerminatorKind::Return { expr } => {
                write!(fmt, "return {}", expr)?;
            }
        }
        Ok(fmt)
    }
}

impl Debug for BasicBlockData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("bb")
            .field("stmts", &self.statements)
            .field("term", &self.terminator)
            .finish()
    }
}

impl Debug for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}
impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            StatementKind::Assign(lhs, rhs) => write!(f, "{} = {}", lhs, rhs),
            StatementKind::Modify(lhs, op, rhs) => write!(f, "{} {}= {}", lhs, op, rhs),
        }
    }
}

impl Display for Lvalue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lvalue::Variable(x) => write!(f, "{}", x.name),
            Lvalue::Temp(x) => write!(f, "t{}", x.0),
        }
    }
}
impl Display for Rvalue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Rvalue::Variable(x) => write!(f, "{}", x.name),
            Rvalue::Temp(x) => write!(f, "t{}", x.0),
            Rvalue::IntegerLit(x) => write!(f, "{}", x),
            Rvalue::BinaryExpr(op, lhs, rhs) => write!(f, "{} {} {}", lhs, op, rhs),
        }
    }
}

impl Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Consume(x) => write!(f, "{}", x),
            Operand::IntegerLit(x) => write!(f, "{}", x),
            Operand::Unit => write!(f, "()"),
        }
    }
}

impl Display for ExprOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprOperator::Add => write!(f, "+"),
            ExprOperator::Sub => write!(f, "-"),
            ExprOperator::Mul => write!(f, "*"),
            ExprOperator::Div => write!(f, "/"),
            ExprOperator::Eq => write!(f, "=="),
            ExprOperator::Lt => write!(f, "<"),
            ExprOperator::Gt => write!(f, ">"),
            ExprOperator::Lte => write!(f, "<="),
            ExprOperator::Gte => write!(f, ">="),
        }
    }
}

#[derive(Default)]
pub struct DebugString(String);

impl Display for DebugString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl Debug for DebugString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl Write for DebugString {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.0.push_str(s);
        Ok(())
    }
}
