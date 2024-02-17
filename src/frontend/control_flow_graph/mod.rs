use std::fmt::Debug;

use super::ast::{self, Type};

pub mod build;

pub trait CFGBuilder {
    fn parse_function(&mut self, data: ast::FnDeclData) -> CFNodeIndex;
    fn new() -> Self;
    fn parse_program(data: ast::ProgramData) -> Vec<Self>
    where
        Self: Sized,
    {
        let mut graphs = vec![];
        for node in data.body {
            if let ast::NodeType::FnDeclaration(data) = node.node_type {
                let mut graph = Self::new();
                graph.parse_function(data);
                graphs.push(graph);
            }
        }
        graphs
    }
}

#[derive(Debug, Clone)]
pub struct CFGraph {
    pub nodes: Vec<CFNode>,
    pub root: CFNodeIndex,
}

#[derive(Debug, Clone, Copy)]
pub struct CFNodeIndex {
    pub index: usize,
}

impl CFNodeIndex {
    pub fn uninitialized() -> usize {
        usize::MAX
    }
}

#[derive(Debug, Clone)]
pub enum CFNode {
    Start(Vec<CFNode>),
    Node(CFNodeData),
    End,
}

#[derive(Debug, Clone)]
pub struct CFNodeData {
    pub statements: Vec<Statement>,
    pub branches: Vec<CFNodeIndex>,
    pub condition: Option<Statement>,
    pub parent: Option<CFNodeIndex>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    LetDecl(LetDeclData),
    Return(ReturnData),
    BinaryExpr(BinaryExprData),
    Identifier(IdentifierData),
    IntLiteral(usize),
}

#[derive(Debug, Clone)]
pub struct LetDeclData {
    pub var_type: Option<Type>,
    pub var_name: String,
    pub var_value: Option<Box<Statement>>,
}

#[derive(Debug, Clone)]
pub struct AssignData {
    pub var_name: String,
    pub var_decl: CFNodeIndex,
    pub var_value: Box<Statement>,
}

#[derive(Debug, Clone)]
pub struct BinaryExprData {
    pub operator: ExprOperator,
    pub left: Box<Statement>,
    pub right: Box<Statement>,
}

#[derive(Debug, Clone)]
pub struct IdentifierData {
    pub name: String,
    pub var_decl: CFNodeIndex,
    pub var_type: Option<Type>,
}

#[derive(Debug, Clone)]
pub struct ReturnData {
    pub value: Box<Statement>,
}

#[derive(Debug, Clone)]
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

impl CFGraph {
    pub fn allocate_node(&mut self, data: CFNodeData) -> CFNodeIndex {
        let index = self.nodes.len();
        self.nodes.push(CFNode::Node(data));
        CFNodeIndex { index }
    }
    pub fn get_node(&self, index: CFNodeIndex) -> &CFNode {
        &self.nodes[index.index]
    }
    pub fn get_node_mut(&mut self, index: CFNodeIndex) -> &mut CFNode {
        &mut self.nodes[index.index]
    }
}
