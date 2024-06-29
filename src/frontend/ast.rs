use std::fmt::Debug;

use crate::{frontend::lexer::Token, span::Span};

#[derive(Clone, PartialEq)]
pub struct Node {
    pub node_type: NodeType,
    pub span: Span,
}

// TODO: rethink Box<Node> - storing nodes on the heap isn't great.
// use lifetimed reference?
#[derive(Debug, Clone, PartialEq)]
pub enum NodeType {
    Program(ProgramData),
    FnDeclaration(FnDeclData),
    LetDeclaration(LetDeclData),
    IfStatement(IfData),
    ReturnStatement(ReturnData),
    BinaryExpr(BinaryExprData),
    Identifier(IdentifierData),
    IntegerLiteral(usize),
    Semicolon,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Usize,
    U32,
    Identifier(String),
    Unresolved,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IdentifierData {
    pub name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfData {
    pub expr: Box<Node>,
    pub body: Vec<Node>,
    pub else_body: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProgramData {
    pub body: Vec<Node>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnDeclData {
    pub name: String,
    pub body: Vec<Node>,
    pub return_type: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetDeclData {
    pub name: String,
    pub r#type: Type,
    pub expr: Option<Box<Node>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnData {
    pub expr: Option<Box<Node>>,
    // TODO: somehow reference the FnDeclData without engaging in lifetime madness?
    pub fn_name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExprData {
    pub left: Box<Node>,
    pub right: Box<Node>,
    pub operator: Token,
}

impl Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            write!(f, "{:#?}", self.node_type)
        } else {
            write!(f, "{:?}", self.node_type)
        }
    }
}
