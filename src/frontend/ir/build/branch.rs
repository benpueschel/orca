use crate::{
    frontend::{
        ast::{self, IfData, NodeType, ReturnData},
        ir::{BasicBlock, Ir, Operand, Scope, Statement, Terminator, TerminatorKind},
    },
    span::Span,
};

pub struct Branch {
    pub terminator: Terminator,
    pub condition_stmts: Vec<Statement>,
    pub leaves: Vec<BasicBlock>,
}

impl Ir {
    pub fn is_branching(&self, node: &ast::Node) -> bool {
        matches!(
            node.node_type,
            NodeType::IfStatement(_) | NodeType::ReturnStatement(_)
        )
    }

    pub fn traverse_return(&mut self, data: ReturnData, scope: Scope, span: Span) -> Branch {
        let (expr, stmts) = match data.expr {
            Some(expr) => self.traverse_expr(*expr, span, scope),
            None => (Operand::Unit, vec![]),
        };
        Branch {
            terminator: Terminator {
                kind: TerminatorKind::Return { expr },
                scope,
                span,
            },
            condition_stmts: stmts,
            leaves: vec![],
        }
    }

    pub fn traverse_if(&mut self, data: IfData, scope: Scope, span: Span) -> Branch {
        let (condition, condition_stmts) = self.traverse_expr_as_rvalue(*data.expr, scope);

        let then_body = self.traverse_body(data.body, span, Some(scope));
        let else_body = self.traverse_body(data.else_body, span, Some(scope));

        Branch {
            terminator: Terminator {
                kind: TerminatorKind::If {
                    condition,
                    targets: (then_body.0, else_body.0),
                },
                scope,
                span,
            },
            leaves: vec![then_body.1, else_body.1],
            condition_stmts,
        }
    }

    pub fn traverse_branch(&mut self, node: ast::Node, scope: Scope) -> Branch {
        match node.node_type {
            NodeType::ReturnStatement(data) => self.traverse_return(data, scope, node.span),
            NodeType::IfStatement(data) => self.traverse_if(data, scope, node.span),
            _ => panic!("node is not a branch"),
        }
    }
}
