use crate::{
    frontend::{
        ast::{self, NodeType, Type},
        ir::{
            ExprOperator, Lvalue, Operand, Rvalue, Scope, Statement, StatementKind, TempDecl,
            TempVal, Var, VAR_UNINITIALIZED,
        },
        lexer::TokenType,
    },
    span::Span,
};

use super::Ir;

impl Ir {
    pub(super) fn traverse_expr(
        &mut self,
        expr: ast::Node,
        span: Span,
        scope: Scope,
    ) -> (Operand, Vec<Statement>) {
        match expr.node_type {
            NodeType::BinaryExpr(data) => self.traverse_binary_expr(data, span, scope),
            NodeType::Identifier(data) => (
                Operand::Consume(Lvalue::Variable(Var {
                    name: data.name,
                    span: expr.span,
                    var_type: Type::Unresolved,
                    id: VAR_UNINITIALIZED,
                })),
                vec![],
            ),
            _ => panic!("node is not an expression"),
        }
    }
    pub(super) fn traverse_expr_as_rvalue(
        &mut self,
        expr: ast::Node,
        scope: Scope,
    ) -> (Rvalue, Vec<Statement>) {
        match expr.node_type {
            NodeType::BinaryExpr(data) => self.traverse_binary_expr_as_rvalue(data, scope),
            NodeType::Identifier(data) => (
                Rvalue::Variable(Var {
                    name: data.name,
                    span: expr.span,
                    var_type: Type::Unresolved,
                    id: VAR_UNINITIALIZED,
                }),
                vec![],
            ),
            _ => panic!("node is not an expression"),
        }
    }

    pub(super) fn traverse_binary_expr_as_rvalue(
        &mut self,
        data: ast::BinaryExprData,
        scope: Scope,
    ) -> (Rvalue, Vec<Statement>) {
        if let TokenType::Equal = data.operator.token_type {
            panic!("assignment is not a valid expression")
        }

        let (left, mut stmts) = self.traverse_rvalue(*data.left, scope);
        let (right, right_stmts) = self.traverse_rvalue(*data.right, scope);
        stmts.extend(right_stmts);
        let operator = self.get_operator(data.operator.token_type);

        (Rvalue::BinaryExpr(operator, left, right), stmts)
    }

    pub(super) fn traverse_binary_expr(
        &mut self,
        data: ast::BinaryExprData,
        span: Span,
        scope: Scope,
    ) -> (Operand, Vec<Statement>) {
        match data.operator.token_type {
            TokenType::Equal => panic!("assignment is not a valid expression"),
            _ => {
                let (left, mut stmts) = self.traverse_rvalue(*data.left, scope);
                let (right, mut right_stmts) = self.traverse_rvalue(*data.right, scope);
                stmts.append(&mut right_stmts);
                let temp = Lvalue::Temp(self.alloc_temp(scope));
                let operator = self.get_operator(data.operator.token_type);
                stmts.push(Statement {
                    span,
                    kind: StatementKind::Assign(temp.clone(), left),
                });
                stmts.push(Statement {
                    span,
                    kind: StatementKind::Modify(temp.clone(), operator, right),
                });
                (Operand::Consume(temp), stmts)
            }
        }
    }

    pub(super) fn traverse_lvalue(&mut self, node: ast::Node, _scope: Scope) -> Lvalue {
        match node.node_type {
            NodeType::Identifier(data) => Lvalue::Variable(Var {
                name: data.name,
                span: node.span,
                var_type: Type::Unresolved,
                id: VAR_UNINITIALIZED,
            }),
            x => panic!("node {:?} is not an lvalue", x),
        }
    }

    pub(super) fn traverse_rvalue(
        &mut self,
        node: ast::Node,
        scope: Scope,
    ) -> (Operand, Vec<Statement>) {
        match node.node_type {
            NodeType::BinaryExpr(data) => self.traverse_binary_expr(data, node.span, scope),
            NodeType::Identifier(data) => (
                Operand::Consume(Lvalue::Variable(Var {
                    name: data.name,
                    span: node.span,
                    var_type: Type::Unresolved,
                    id: VAR_UNINITIALIZED,
                })),
                vec![],
            ),
            NodeType::IntegerLiteral(data) => (Operand::IntegerLit(data), vec![]),
            x => panic!("node {:?} is not an rvalue", x),
        }
    }

    fn get_operator(&self, token_type: TokenType) -> ExprOperator {
        match token_type {
            TokenType::Plus => ExprOperator::Add,
            TokenType::Minus => ExprOperator::Sub,
            TokenType::Star => ExprOperator::Mul,
            TokenType::Slash => ExprOperator::Div,
            TokenType::LeftCaret => ExprOperator::Lt,
            TokenType::RightCaret => ExprOperator::Gt,
            _ => panic!("token type {:?} is not an operator", token_type),
        }
    }

    fn alloc_temp(&mut self, scope: Scope) -> TempVal {
        let data = self.scope_data_mut(scope);
        data.temp_decls.push(TempDecl { scope });
        TempVal(data.temp_decls.len() - 1)
    }
}
