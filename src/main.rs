use std::{
    fs::File,
    io::{self, BufRead, Write},
    path::Path,
};

use backend::gen::{x86_linux, CodeGenerator};
use structopt::StructOpt;
use target::TargetPlatform;

use frontend::{ast::NodeType, lexer::Lexer, parser::Parser};

use crate::{backend::gen::x86_linux::codegen::generate_code, frontend::control_flow_graph::CFGraph};

pub mod backend;
pub mod error;
pub mod frontend;
pub mod target;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "orca",
    about = "the world's worst compiler for the world's worst language"
)]
struct Options {
    #[structopt(short, long)]
    file: String,
    #[structopt(long)]
    print_assembly: bool,
}

fn main() -> io::Result<()> {
    let opt = Options::from_args();

    let file = read_lines(opt.file)?;
    let mut input = String::new();
    for line in file.flatten() {
        input += &line;
    }

    // TODO: parse target platform from options
    let target = TargetPlatform::LinuxX86_64;

    // TODO: parse tree on demand - we'd need to pass the BufReader down to the lexer
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let ast = match parser.produce_ast() {
        Ok(x) => x,
        Err(x) => panic!("{:?}", x),
    };

    let generator = x86_linux::X86Linux::new(ast);
    let assembly = generate_code(generator.nodes);

    if opt.print_assembly {
        println!("{}", assembly);
    } else {
        let temp_dir = ".orca.tmp";
        std::fs::create_dir(temp_dir)?;

        let asm_file = format!("{}/a.a", temp_dir);
        let o_file = format!("{}/a.o", temp_dir);
        let out_file = format!("a.out");

        std::fs::File::create(&asm_file)?.write_all(assembly.as_bytes())?;

        backend::assembler::assemble(target, &asm_file, &o_file)?;
        backend::linker::link(target, &o_file, &out_file)?;

        std::fs::remove_dir_all(temp_dir)?;
    }

    Ok(())
}

// The output is wrapped in a Result to allow matching on errors.
// Returns an Iterator to the Reader of the lines of the file.
fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}
