use crate::frontend::{
    ast::Type,
    ir::{
        BasicBlock, Ir, Lvalue, Operand, Rvalue, Statement, StatementKind, TerminatorKind,
        Var, IR_START_BLOCK,
    },
};

pub fn resolve_types(mut ir: Ir) -> Ir {
    resolve_types_mut(&mut ir);
    ir
}

pub fn resolve_types_mut(ir: &mut Ir) {
    resolve_types_in_block(ir, IR_START_BLOCK);
}

fn resolve_types_in_block(ir: &mut Ir, block: BasicBlock) {
    // NOTE: this is safe because we're not modifying the IR, just the symbol types
    let block = unsafe { (*(ir as *mut Ir)).basic_block_data_mut(block) };
    for statement in &mut block.statements {
        resolve_types_in_statement(ir, statement);
    }
    if let Some(terminator) = &mut block.terminator {
        match &mut terminator.kind {
            TerminatorKind::Goto { target } => resolve_types_in_block(ir, *target),
            TerminatorKind::If { condition, targets } => {
                resolve_types_in_rvalue(ir, condition);
                resolve_types_in_block(ir, targets.0);
                resolve_types_in_block(ir, targets.1);
            }
            TerminatorKind::Return { expr } => {
                resolve_types_in_operand(ir, expr);
            }
        }
    }
}

fn resolve_types_in_statement(ir: &mut Ir, statement: &mut Statement) {
    match &mut statement.kind {
        StatementKind::Assign(lvalue, operand) => {
            resolve_types_in_lvalue(ir, lvalue);
            resolve_types_in_operand(ir, operand);
        }
        StatementKind::Modify(lvalue, _, operand) => {
            resolve_types_in_lvalue(ir, lvalue);
            resolve_types_in_operand(ir, operand);
        }
    }
}

fn resolve_types_in_rvalue(ir: &mut Ir, rvalue: &mut Rvalue) {
    match rvalue {
        Rvalue::IntegerLit(_) => {}
        Rvalue::Variable(var) => {
            resolve_type(ir, var);
        }
        Rvalue::Temp(_temp) => { /* TODO: tesolve temp type */ }
        Rvalue::BinaryExpr(_, lhs, rhs) => {
            resolve_types_in_operand(ir, lhs);
            resolve_types_in_operand(ir, rhs);
        }
    }
}

fn resolve_types_in_lvalue(ir: &mut Ir, lvalue: &mut Lvalue) {
    match lvalue {
        Lvalue::Variable(var) => {
            resolve_type(ir, var);
        }
        Lvalue::Temp(_) => { /* TODO: resolve temp type */ }
    }
}

fn resolve_types_in_operand(ir: &mut Ir, operand: &mut Operand) {
    match operand {
        Operand::Consume(lvalue) => {
            resolve_types_in_lvalue(ir, lvalue);
        }
        Operand::Unit => {}
        Operand::IntegerLit(_) => {}
    }
}

fn resolve_type(ir: &mut Ir, var: &mut Var) {
    let decl = ir.find_declaration(var);
    if decl.var.var_type == Type::Unresolved {
        assert!(
            var.var_type == Type::Unresolved,
            "cannot infer type for variable {}",
            var.name
        );
        decl.var.var_type = var.var_type.clone();
    } else if decl.var.var_type != var.var_type {
        if var.var_type == Type::Unresolved {
            var.var_type = decl.var.var_type.clone();
            return;
        }
        panic!(
            "type mismatch: expected {:?}, found {:?} (for variable {:?})",
            decl.var.var_type, var.var_type, var
        );
    }
}
