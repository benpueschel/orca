use crate::frontend::ast::{Node, Type};

use self::x86_linux::assembly_node::Expression;

pub mod x86_linux;

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    identifier: String,
    r#type: Type,
    current_location: Expression,
    memory_location: Option<Expression>,
}

pub trait CodeGenerator {
    //fn new(graph: CFGraph) -> Self;
    fn new(node: Node) -> Self;
}
