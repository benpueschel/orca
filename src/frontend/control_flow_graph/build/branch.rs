use crate::frontend::{ast, control_flow_graph::{CFGraph, CFNodeIndex}};

impl CFGraph {
    pub(super) fn is_branching(&self, node_type: &ast::NodeType) -> bool {
        match node_type {
            ast::NodeType::IfStatement(_) => true,
            _ => false,
        }
    }
    pub(super) fn parse_branch(&mut self, node: ast::Node) -> Vec<CFNodeIndex> {
        match node.node_type {
            ast::NodeType::IfStatement(data) => self.parse_if_statement(data),
            _ => unimplemented!(),
        }
    }
    
    fn parse_if_statement(&mut self, data: ast::IfData) -> Vec<CFNodeIndex> {
        let body = self.parse_nodes(data.body.into());
        let else_body = self.parse_nodes(data.else_body.into());
        vec![body, else_body]
    }
}
