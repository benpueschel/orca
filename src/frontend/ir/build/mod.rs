use crate::{
    frontend::{
        ast::{self, NodeType},
        ir::{StatementKind, TerminatorKind, IR_END_BLOCK, IR_START_BLOCK},
        lexer::TokenType,
    },
    span::Span,
};

pub struct BasicBlockBuilder {
    pub block: BasicBlock,
    pub leaves: Vec<BasicBlock>,
}

use super::{
    BasicBlock, BasicBlockData, Ir, Lvalue, Operand, Scope, ScopeData, Statement, Terminator, Var,
    VarDecl,
};

pub mod branch;
pub mod expr;

pub struct Builder {}
impl Builder {
    #[allow(unused)]
    pub fn build(data: ast::FnDeclData, span: Span) -> Ir {
        let mut ir = Ir {
            basic_blocks: vec![],
            var_decls: vec![],
            temp_decls: vec![],
            scopes: vec![],
            span,
        };
        assert_eq!(ir.alloc_empty_basic_block(), IR_START_BLOCK);
        assert_eq!(ir.alloc_empty_basic_block(), IR_END_BLOCK);

        let (first_node, last_node) = ir.traverse_body(data.body, span, None);

        ir.basic_block_data_mut(IR_START_BLOCK).terminator = Some(Terminator {
            kind: TerminatorKind::Goto { target: first_node },
            scope: Scope(0), // TODO: global scope
            span,
        });

        ir.basic_block_data_mut(last_node).terminator = Some(Terminator {
            kind: TerminatorKind::Goto {
                target: IR_END_BLOCK,
            },
            scope: Scope(0), // TODO: global scope
            span,
        });
        ir
    }
}

impl Ir {
    fn traverse_body(
        &mut self,
        body: Vec<ast::Node>,
        span: Span,
        parent_scope: Option<Scope>,
    ) -> (BasicBlock, BasicBlock) {
        let scope = self.alloc_scope(ScopeData {
            span,
            parent: parent_scope,
        });
        let mut statements = vec![];
        let mut blocks = vec![];

        for node in body {
            if self.is_branching(&node) {
                let branch = self.traverse_branch(node, scope);
                statements.extend(branch.condition_stmts);

                let block = self.alloc_basic_block(BasicBlockData {
                    statements,
                    terminator: Some(branch.terminator),
                });

                if let Some(last_block) = blocks.last() {
                    self.terminate_leaves(last_block, block, scope);
                }

                blocks.push(BasicBlockBuilder {
                    block,
                    leaves: branch.leaves,
                });
                statements = Vec::new();
            } else {
                statements.extend(self.traverse_statement(node, scope));
            }
        }

        let last_block = self.alloc_basic_block(BasicBlockData {
            statements,
            terminator: None,
        });

        if let Some(block) = blocks.last() {
            self.terminate_leaves(block, last_block, scope);
        }

        let first_block = blocks.first().map(|x| x.block).unwrap_or(last_block);
        (first_block, last_block)
    }

    fn terminate_leaves(&mut self, block: &BasicBlockBuilder, target: BasicBlock, scope: Scope) {
        for leaf in &block.leaves {
            let term = &mut self.basic_block_data_mut(*leaf).terminator;
            if let None = term {
                *term = Some(Terminator {
                    kind: TerminatorKind::Goto { target },
                    scope,
                    span: Span::empty(),
                });
            }
        }
    }

    fn traverse_let_decl(
        &mut self,
        data: ast::LetDeclData,
        span: Span,
        scope: Scope,
    ) -> Vec<Statement> {
        self.var_decls.push(VarDecl {
            name: data.name.clone(),
            span,
            scope,
        });
        if let Some(expr) = data.expr {
            let (right, mut statements) = self.traverse_rvalue(*expr, scope);
            statements.push(Statement {
                kind: StatementKind::Assign(
                    Lvalue::Variable(Var { name: data.name }),
                    right.into(),
                ),
                span,
            });
            return statements;
        }
        Vec::new()
    }

    fn traverse_statement(&mut self, node: ast::Node, scope: Scope) -> Vec<Statement> {
        println!("traverse_statement {:?}", node);
        match node.node_type {
            NodeType::LetDeclaration(data) => self.traverse_let_decl(data, node.span.into(), scope),
            NodeType::BinaryExpr(data) => {
                // we are only interested in assignments because this function is only called for
                // statements. any other expression will not have any effect on the program state.
                if let TokenType::Equal = data.operator.token_type {
                    let left = self.traverse_lvalue(*data.left, scope);
                    let (right, mut statements) = self.traverse_rvalue(*data.right, scope);
                    statements.push(Statement {
                        kind: StatementKind::Assign(left, right.into()),
                        span: node.span.into(),
                    });
                    return statements;
                }
                Vec::new()
            }
            NodeType::ReturnStatement(data) => {
                if let Some(expr) = data.expr {
                    let (operand, mut statements) =
                        self.traverse_expr(*expr, node.span.into(), scope);
                    statements.push(Statement {
                        kind: StatementKind::Return(operand),
                        span: node.span.into(),
                    });
                    statements
                } else {
                    vec![Statement {
                        kind: StatementKind::Return(Operand::Unit),
                        span: node.span.into(),
                    }]
                }
            }
            NodeType::Semicolon => vec![],
            x => panic!("node {:?} is not a statement", x),
        }
    }

    pub fn alloc_basic_block(&mut self, data: BasicBlockData) -> BasicBlock {
        let index = self.basic_blocks.len();
        self.basic_blocks.push(data);
        BasicBlock(index)
    }
    pub fn alloc_empty_basic_block(&mut self) -> BasicBlock {
        self.alloc_basic_block(BasicBlockData {
            statements: vec![],
            terminator: None,
        })
    }
    fn alloc_scope(&mut self, data: ScopeData) -> Scope {
        let index = self.scopes.len();
        self.scopes.push(data);
        Scope(index)
    }
}
