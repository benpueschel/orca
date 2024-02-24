use crate::frontend::ir::{
    BasicBlock, ExprOperator, Ir, Lvalue, Operand, Rvalue, Terminator, TerminatorKind,
};

use super::{
    assembly_node::{AssemblyNode, Expression, Instruction, JumpCondition},
    scratch, X86Linux,
};

impl X86Linux {
    pub(super) fn process_terminator(&mut self, graph: &mut Ir, terminator: Terminator) {
        match &terminator.kind {
            TerminatorKind::Return { expr } => self.process_return(expr),
            TerminatorKind::Goto { target } => self.process_basic_block(graph, *target),
            TerminatorKind::If { condition, targets } => self.process_if(graph, condition, targets),
        }
    }

    fn process_return(&mut self, expr: &Operand) {
        let operand = self.process_operand(expr);
        if operand != Expression::None {
            self.nodes.push(AssemblyNode {
                instruction: Instruction::Mov,
                size: 8, // TODO: dynamic size
                left: operand,
                right: Expression::Register(scratch::RAX),
            });
        }
        self.nodes.push(AssemblyNode {
            instruction: Instruction::Jmp(JumpCondition::None),
            size: 0,
            left: Expression::Label(self.finished_label.clone()),
            right: Expression::None,
        });
    }

    fn process_if(
        &mut self,
        graph: &mut Ir,
        condition: &Rvalue,
        targets: &(BasicBlock, BasicBlock),
    ) {
        let then_label = self.label_alloc();
        let done_label = self.label_alloc();
        match condition {
            Rvalue::Temp(temp) => {
                let operand = self.process_operand(&Operand::Consume(Lvalue::Temp(*temp)));
                self.nodes.push(AssemblyNode {
                    instruction: Instruction::Cmp,
                    size: 8, // TODO: dynamic size
                    left: Expression::IntegerLiteral(0),
                    right: operand,
                });
                self.nodes.push(AssemblyNode {
                    instruction: Instruction::Jmp(JumpCondition::Equal),
                    size: 0,
                    left: then_label.clone(),
                    right: Expression::None,
                });
            }
            Rvalue::Variable(var) => {
                let operand =
                    self.process_operand(&Operand::Consume(Lvalue::Variable(var.clone())));
                self.nodes.push(AssemblyNode {
                    instruction: Instruction::Cmp,
                    size: 8, // TODO: dynamic size
                    left: Expression::IntegerLiteral(0),
                    right: operand,
                });
                self.nodes.push(AssemblyNode {
                    instruction: Instruction::Jmp(JumpCondition::Equal),
                    size: 0,
                    left: then_label.clone(),
                    right: Expression::None,
                });
            }
            Rvalue::IntegerLit(lit) => {
                let operand = self.process_operand(&Operand::IntegerLit(*lit));
                self.nodes.push(AssemblyNode {
                    instruction: Instruction::Cmp,
                    size: 8, // TODO: dynamic size
                    left: Expression::IntegerLiteral(0),
                    right: operand,
                });
                self.nodes.push(AssemblyNode {
                    instruction: Instruction::Jmp(JumpCondition::Equal),
                    size: 0,
                    left: then_label.clone(),
                    right: Expression::None,
                });
            }
            Rvalue::BinaryExpr(op, lhs, rhs) => {
                if let Some(condition) = self.get_jump_condition(op) {
                    let left = self.process_operand(rhs);
                    let right = match self.process_operand(lhs) {
                        Expression::Register(x) => Expression::Register(x),
                        x => {
                            self.nodes.push(AssemblyNode {
                                instruction: Instruction::Mov,
                                size: 8, // TODO: dynamic size
                                left: x,
                                right: Expression::Register(scratch::RAX),
                            });
                            Expression::Register(scratch::RAX)
                        }
                    };
                    self.nodes.push(AssemblyNode {
                        instruction: Instruction::Cmp,
                        size: 8, // TODO: dynamic size
                        left,
                        right,
                    });
                    self.nodes.push(AssemblyNode {
                        instruction: Instruction::Jmp(condition),
                        size: 0,
                        left: then_label.clone(),
                        right: Expression::None,
                    });
                } else {
                    let expr = self.process_binary_expr(op.clone(), rhs.clone(), lhs.clone());
                    self.nodes.push(AssemblyNode {
                        instruction: Instruction::Cmp,
                        size: 8, // TODO: dynamic size
                        left: Expression::IntegerLiteral(0),
                        right: expr,
                    });
                    self.nodes.push(AssemblyNode {
                        instruction: Instruction::Jmp(JumpCondition::Equal),
                        size: 0,
                        left: then_label.clone(),
                        right: Expression::None,
                    });
                }
            }
        }

        self.process_basic_block(graph, targets.1);

        self.nodes.push(AssemblyNode {
            instruction: Instruction::Jmp(JumpCondition::None),
            size: 0,
            left: done_label.clone(),
            right: Expression::None,
        });
        self.nodes.push(AssemblyNode {
            instruction: Instruction::LabelDeclaration,
            size: 0,
            left: then_label,
            right: Expression::None,
        });

        self.process_basic_block(graph, targets.0);

        self.nodes.push(AssemblyNode {
            instruction: Instruction::LabelDeclaration,
            size: 0,
            left: done_label,
            right: Expression::None,
        });
    }

    fn get_jump_condition(&self, operator: &ExprOperator) -> Option<JumpCondition> {
        match operator {
            ExprOperator::Eq => Some(JumpCondition::Equal),
            ExprOperator::Gt => Some(JumpCondition::Greater),
            ExprOperator::Gte => Some(JumpCondition::GreaterOrEqual),
            ExprOperator::Lt => Some(JumpCondition::Less),
            ExprOperator::Lte => Some(JumpCondition::LessOrEqual),
            _ => None,
        }
    }
}
