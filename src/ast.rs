use crate::lexer::Token;

// TODO: rethink Box<Node> - storing nodes on the heap isn't great.
// use lifetimed reference?
#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Program(ProgramData),
    FnDeclaration(FnDeclData),
    LetDeclaration(LetDeclData),
    IfStatement(IfData),
    ReturnStatement(ReturnData),
    BinaryExpr(BinaryExprData),
    Identifier(String),
    IntegerLiteral(usize),
    Semicolon,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Usize,
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetDeclData {
    pub name: String,
    pub r#type: Option<Type>,
    pub expr: Option<Box<Node>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnData {
    pub expr: Option<Box<Node>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExprData {
    pub left: Box<Node>,
    pub right: Box<Node>,
    pub operator: Token,
}
