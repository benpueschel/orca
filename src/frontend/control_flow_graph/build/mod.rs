use std::collections::VecDeque;

use crate::frontend::ast;

use super::{CFGBuilder, CFGraph, CFNodeData, CFNodeIndex, Statement};

pub mod branch;
pub mod statement;

impl CFGBuilder for CFGraph {
    fn new() -> CFGraph {
        CFGraph {
            nodes: vec![],
            root: CFNodeIndex { index: CFNodeIndex::uninitialized() },
        }
    }
    fn parse_function(&mut self, data: ast::FnDeclData) -> CFNodeIndex {
        self.parse_nodes(data.body.into())
    }
}

impl CFGraph {
    fn parse_nodes(&mut self, mut nodes: VecDeque<ast::Node>) -> CFNodeIndex {
        let mut statements = vec![];
        let mut branches = vec![];
        let mut condition = None;
        while nodes.len() > 0 {
            let node = nodes.pop_front().unwrap();
            if self.is_branching(&node.node_type) {

                condition = self.parse_condition(&node);
                branches.append(&mut self.parse_branch(node)); // TODO: figure stuff out
                break;
            }
            statements.push(self.parse_statement(node));
        }
        self.allocate_node(CFNodeData {
            statements,
            branches,
            condition,
            parent: None,
        })
    }
    fn parse_condition(&mut self, node: &ast::Node) -> Option<Statement> {
        match &node.node_type {
            ast::NodeType::IfStatement(data) => Some(self.parse_statement(*data.expr.clone())),
            _ => None,
        }
    }
}
