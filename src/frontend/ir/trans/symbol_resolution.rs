use crate::frontend::ir::{
    BasicBlock, Ir, Lvalue, Operand, Rvalue, Scope, Statement, StatementKind, TerminatorKind, Var,
    IR_START_BLOCK,
};

pub fn resolve_symbols(mut ir: Ir) -> Ir {
    resolve_symbols_mut(&mut ir);
    ir
}

pub fn resolve_symbols_mut(ir: &mut Ir) {
    resolve_symbols_in_block(ir, IR_START_BLOCK);
}
fn resolve_symbols_in_block(ir: &mut Ir, block: BasicBlock) {
    // NOTE: this is safe because we're not modifying the IR, just the symbol IDs
    let block = unsafe { (*(ir as *mut Ir)).basic_block_data_mut(block) };
    for statement in &mut block.statements {
        resolve_symbols_in_statement(ir, statement, block.scope);
    }
    if let Some(terminator) = &mut block.terminator {
        match &mut terminator.kind {
            TerminatorKind::Goto { target } => resolve_symbols_in_block(ir, *target),
            TerminatorKind::If { condition, targets } => {
                resolve_symbols_in_rvalue(ir, condition, block.scope);
                resolve_symbols_in_block(ir, targets.0);
                resolve_symbols_in_block(ir, targets.1);
            }
            TerminatorKind::Return { expr } => {
                resolve_symbols_in_operand(ir, expr, block.scope);
            }
        }
    }
}
fn resolve_symbols_in_statement(ir: &mut Ir, statement: &mut Statement, scope: Scope) {
    match &mut statement.kind {
        StatementKind::Assign(lvalue, operand) => {
            resolve_symbols_in_lvalue(ir, lvalue, scope);
            resolve_symbols_in_operand(ir, operand, scope);
        }
        StatementKind::Modify(lvalue, _, operand) => {
            resolve_symbols_in_lvalue(ir, lvalue, scope);
            resolve_symbols_in_operand(ir, operand, scope);
        }
    }
}
fn resolve_symbols_in_rvalue(ir: &mut Ir, rvalue: &mut Rvalue, scope: Scope) {
    match rvalue {
        Rvalue::Variable(var) => resolve_symbol(ir, var, scope),
        Rvalue::IntegerLit(_) => {}
        Rvalue::Temp(_) => {}
        Rvalue::BinaryExpr(_, lhs, rhs) => {
            resolve_symbols_in_operand(ir, lhs, scope);
            resolve_symbols_in_operand(ir, rhs, scope);
        }
    }
}
fn resolve_symbols_in_lvalue(ir: &mut Ir, lvalue: &mut Lvalue, scope: Scope) {
    match lvalue {
        Lvalue::Variable(var) => {
            resolve_symbol(ir, var, scope);
        }
        Lvalue::Temp(_) => {}
    }
}
fn resolve_symbols_in_operand(ir: &mut Ir, operand: &mut Operand, scope: Scope) {
    match operand {
        Operand::Consume(lvalue) => {
            resolve_symbols_in_lvalue(ir, lvalue, scope);
        }
        Operand::Unit => {}
        Operand::IntegerLit(_) => {}
    }
}

fn resolve_symbol(ir: &mut Ir, var: &mut Var, scope_index: Scope) {
    let scope = ir.scope_data_mut(scope_index);
    for i in 0..scope.var_decls.len() {
        // if we find the symbol in this scope, assign it an ID:
        // the ID is a 64-bit integer with the high 32 bits being the scope index 
        // and the low 32 bits being the index of the symbol in the scope
        if scope.var_decls[i].var.name == var.name {
            let id = scope_index.0 << 32 | i;
            scope.var_decls[i].var.id = id;
            var.id = id;
            return;
        }
    }
    // if we didn't find the symbol in this scope, try the parent scope
    if let Some(parent) = scope.parent {
        resolve_symbol(ir, var, parent);
        return;
    }
    panic!("Symbol not found: {} {}", var.name, var.span);
}
