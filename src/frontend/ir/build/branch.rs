use crate::{
    frontend::{
        ast::{self, NodeType},
        ir::{BasicBlock, Ir, Scope, Statement, Terminator, TerminatorKind},
    },
    span::Span,
};
use crate::frontend::ir::BasicBlockData;

pub struct Branch {
    pub terminator: Terminator,
    pub condition_stmts: Vec<Statement>,
    pub leaves: Vec<BasicBlock>,
}

impl Ir {
    pub fn is_branching(&self, node: &ast::Node) -> bool {
        match node.node_type {
            NodeType::IfStatement(_) => true,
            _ => false,
        }
    }

    pub fn traverse_branch(&mut self, node: ast::Node, scope: Scope) -> Branch {
        println!("traverse_branch {:?}", node.span);
        println!("node {:?}", node.node_type);
        match node.node_type {
            NodeType::IfStatement(data) => {
                let (condition, condition_stmts) =
                    self.traverse_expr(*data.expr, node.span.into(), scope);

                let then_body = self.traverse_body(data.body, node.span.into(), Some(scope));
                let else_body = self.traverse_body(data.else_body, node.span.into(), Some(scope));

                Branch {
                    terminator: Terminator {
                        scope,
                        span: node.span.into(),
                        kind: TerminatorKind::If {
                            condition,
                            targets: (then_body.0, else_body.0),
                        },
                    },
                    leaves: vec![then_body.1, else_body.1],
                    condition_stmts,
                }
            }
            _ => panic!("node is not a branch"),
        }
    }
}
