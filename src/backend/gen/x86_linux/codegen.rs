use super::{
    assembly_node::{AssemblyNode, Expression, Instruction, JumpCondition},
    scratch::ScratchRegisters,
};

pub fn generate_code(nodes: &Vec<AssemblyNode>) -> String {
    let mut code = String::new();
    for node in nodes {
        match node.instruction {
            Instruction::LabelDeclaration => {
                let label = match &node.left {
                    Expression::Label(x) => x,
                    _ => panic!("LabelDeclaration must have a label"),
                };
                code.push_str(&format!("{}:\n", label));
            }
            Instruction::AssemblyDirective => {
                let directive = match &node.left {
                    Expression::Label(x) => x,
                    _ => panic!("AssemblyDirective must have a label"),
                };
                code.push_str(&format!("{}\n", directive));
            }
            _ => {
                let instruction_name = get_instruction_name(&node.instruction, node.size);
                if let Expression::None = node.left {
                    code.push_str(&format!("{}\n", instruction_name));
                    continue;
                }
                if let Expression::None = node.right {
                    code.push_str(&format!(
                        "{} {}\n",
                        instruction_name,
                        get_expression(&node.left, node.size)
                    ));
                    continue;
                }
                let left = get_expression(&node.left, node.size);
                let right = get_expression(&node.right, node.size);
                code.push_str(&format!("{} {}, {}\n", instruction_name, left, right));
            }
        }
    }
    code
}

fn get_expression(expression: &Expression, size: usize) -> String {
    match expression {
        Expression::IntegerLiteral(x) => format!("${}", x),
        Expression::Register(reg) => format!("{}", ScratchRegisters::get_name(*reg, size)),
        Expression::Memory(x) => x.clone(),
        Expression::Label(x) => format!("{}", x),
        Expression::None => panic!("Expression::None is not a valid expression"),
    }
}

fn get_instruction_name(instruction: &Instruction, type_size: usize) -> String {
    match type_size {
        0 => get_instruction_name_0(instruction).into(),
        1 => get_instruction_name_8(instruction).into(),
        2 => get_instruction_name_16(instruction).into(),
        4 => get_instruction_name_32(instruction).into(),
        8 => get_instruction_name_64(instruction).into(),
        _ => panic!("Invalid type size"),
    }
}

fn get_instruction_name_0(instruction: &Instruction) -> impl Into<String> {
    match instruction {
        Instruction::Jmp(x) => match x {
            JumpCondition::Less => "jl",
            JumpCondition::LessOrEqual => "jle",
            JumpCondition::Greater => "jg",
            JumpCondition::GreaterOrEqual => "jge",
            JumpCondition::Equal => "je",
            JumpCondition::NotEqual => "jne",
            JumpCondition::None => "jmp",
        },
        Instruction::Ret => "ret",
        Instruction::AssemblyDirective => panic!("AssemblyDirective is not a valid instruction"),
        Instruction::LabelDeclaration => panic!("LabelDeclaration is not a valid instruction"),
        x => panic!("instruction {:?} is not supported for size 0", x),
    }
}

fn get_instruction_name_8(instruction: &Instruction) -> impl Into<String> {
    match instruction {
        Instruction::Mov => "movb",
        Instruction::Cmp => "cmpb",
        Instruction::Add => "addb",
        Instruction::Sub => "sub",
        Instruction::IMul => "imulb",
        Instruction::IDiv => "idivb",
        Instruction::And => "andb",
        Instruction::Or => "orb",
        Instruction::Xor => "xorb",
        Instruction::Push => "pushb",
        Instruction::Pop => "popb",
        Instruction::Ret => "ret",
        Instruction::Set(x) => match x {
            JumpCondition::Less => "setl",
            JumpCondition::LessOrEqual => "setle",
            JumpCondition::Greater => "setg",
            JumpCondition::GreaterOrEqual => "setge",
            JumpCondition::Equal => "sete",
            JumpCondition::NotEqual => "setne",
            JumpCondition::None => panic!("Set with None is not a valid instruction"),
        },
        Instruction::Jmp(_) => panic!("Jmp is not a valid instruction for 8 bit"),
        Instruction::LabelDeclaration => panic!("LabelDeclaration is not a valid instruction"),
        Instruction::AssemblyDirective => panic!("AssemblyDirective is not a valid instruction"),
    }
}
fn get_instruction_name_16(instruction: &Instruction) -> impl Into<String> {
    match instruction {
        Instruction::Mov => "movw",
        Instruction::Cmp => "cmpw",
        Instruction::Add => "addw",
        Instruction::Sub => "subw",
        Instruction::IMul => "imulw",
        Instruction::IDiv => "idivw",
        Instruction::And => "andw",
        Instruction::Or => "orw",
        Instruction::Xor => "xorw",
        Instruction::Push => "pushw",
        Instruction::Pop => "popw",
        Instruction::Ret => "ret",
        Instruction::Set(_) => panic!("Set is not a valid instruction for 16 bit"),
        Instruction::Jmp(_) => panic!("Jmp is not a valid instruction for 16 bit"),
        Instruction::LabelDeclaration => panic!("LabelDeclaration is not a valid instruction"),
        Instruction::AssemblyDirective => panic!("AssemblyDirective is not a valid instruction"),
    }
}
fn get_instruction_name_32(instruction: &Instruction) -> impl Into<String> {
    match instruction {
        Instruction::Mov => "movl",
        Instruction::Cmp => "cmpl",
        Instruction::Add => "addl",
        Instruction::Sub => "subl",
        Instruction::IMul => "imull",
        Instruction::IDiv => "idivl",
        Instruction::And => "andl",
        Instruction::Or => "orl",
        Instruction::Xor => "xorl",
        Instruction::Push => "pushl",
        Instruction::Pop => "popl",
        Instruction::Ret => "ret",
        Instruction::Set(_) => panic!("Set is not a valid instruction for 32 bit"),
        Instruction::Jmp(_) => panic!("Jmp is not a valid instruction for 32 bit"),
        Instruction::LabelDeclaration => panic!("LabelDeclaration is not a valid instruction"),
        Instruction::AssemblyDirective => panic!("AssemblyDirective is not a valid instruction"),
    }
}
fn get_instruction_name_64(instruction: &Instruction) -> impl Into<String> {
    match instruction {
        Instruction::Mov => "movq",
        Instruction::Cmp => "cmpq",
        Instruction::Add => "addq",
        Instruction::Sub => "subq",
        Instruction::IMul => "imulq",
        Instruction::IDiv => "idivq",
        Instruction::And => "andq",
        Instruction::Or => "orq",
        Instruction::Xor => "xorq",
        Instruction::Push => "pushq",
        Instruction::Pop => "popq",
        Instruction::Ret => "ret",
        Instruction::Set(_) => panic!("Set is not a valid instruction for 64 bit"),
        Instruction::Jmp(_) => panic!("Jmp is not a valid instruction for 64 bit"),
        Instruction::LabelDeclaration => panic!("LabelDeclaration is not a valid instruction"),
        Instruction::AssemblyDirective => panic!("AssemblyDirective is not a valid instruction"),
    }
}
