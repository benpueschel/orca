use crate::frontend::{ast::Type, ir::Ir};

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
    fn new() -> Self;
    fn process_graph(&mut self, graph: Ir);
}
