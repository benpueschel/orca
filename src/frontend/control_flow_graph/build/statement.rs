use crate::frontend::{
    ast::{self},
    control_flow_graph::{BinaryExprData, ExprOperator, CFGraph, Statement, LetDeclData},
};

impl CFGraph {
    pub(super) fn parse_statement(&mut self, node: ast::Node) -> Statement {
        match node.node_type {
            ast::NodeType::LetDeclaration(data) => self.parse_let_decl(data),
            ast::NodeType::BinaryExpr(data) => self.parse_binary_expr(data),
            _ => panic!("node {:?} is not a statement", node),
        }
    }
    fn parse_let_decl(&mut self, data: ast::LetDeclData) -> Statement {
        let var_value = match data.expr {
            Some(value) => Some(Box::new(self.parse_statement(*value))),
            None => None,
        };
        Statement::LetDecl(LetDeclData {
            var_type: None,
            var_name: data.name,
            var_value,
        })
    }
    fn parse_binary_expr(&mut self, data: ast::BinaryExprData) -> Statement {
        let left = Box::new(self.parse_statement(*data.left));
        let right = Box::new(self.parse_statement(*data.right));
        Statement::BinaryExpr(BinaryExprData {
            left,
            right,
            operator: ExprOperator::Add, //TODO: parse operator
        })
    }
}
