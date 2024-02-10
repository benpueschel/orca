use std::{
    fs::File,
    io::{self, BufRead, Write},
    path::Path,
    process::{Command, ExitCode, ExitStatus, Output},
};

use gen::create_code_generator;
use structopt::StructOpt;
use target::TargetPlatform;

use crate::{lexer::Lexer, parser::Parser};

pub mod ast;
pub mod error;
pub mod gen;
pub mod lexer;
pub mod parser;
pub mod target;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "orca",
    about = "the world's worst compiler for the world's worst language"
)]
struct Options {
    #[structopt(short, long)]
    file: String,
}

fn main() -> io::Result<()> {
    let opt = Options::from_args();
    let file = read_lines(opt.file)?;
    let mut input = String::new();
    for line in file.flatten() {
        input += &line;
    }

    // TODO: parse tree on demand - we'd need to pass the BufReader down to the lexer
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let ast = match parser.produce_ast() {
        Ok(x) => x,
        Err(x) => panic!("{:?}", x),
    };

    let mut generator = create_code_generator(TargetPlatform::LinuxX86_64);
    let assembly = match generator.generate_assembly(&ast) {
        Ok(x) => x,
        Err(x) => panic!("{:?}", x),
    };

    let temp_dir = ".orca.tmp";
    std::fs::create_dir(temp_dir)?;

    let asm_file = format!("{}/a.a", temp_dir);
    let o_file = format!("{}/a.o", temp_dir);
    let out_file = format!("a.out");

    // $ as -o hello.o hello.s
    //$ ld -o hello -dynamic-linker /lib/ld-linux.so.2 /usr/lib/crt1.o /usr/lib/crti.o -lc hello.o /usr/lib/crtn.o

    std::fs::File::create(&asm_file)?.write_all(assembly.as_bytes())?;
    process_output(
        Command::new("as")
            .args(["-o", &o_file, &asm_file])
            .output()?,
    );
    process_output(
        Command::new("ld")
            .args([
                "-o",
                &out_file,
                "-dynamic-linker",
                "/lib64/ld-linux-x86-64.so.2",
                "/usr/lib/crt1.o",
                "/usr/lib/crti.o",
                "-lc",
                &o_file,
                "/usr/lib/crtn.o",
            ])
            .output()?,
    );

    std::fs::remove_dir_all(temp_dir)?;

    Ok(())
}

fn process_output(output: Output) {
    if output.status.success() {
        println!(
            "{}",
            String::from_utf8(output.stdout).expect("could not parse stdout")
        );
    } else {
        println!(
            "{}",
            String::from_utf8(output.stderr).expect("could not parse stderr")
        );
    }
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
