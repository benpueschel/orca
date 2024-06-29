# orca

A learning project implementing a basic compiler in Rust.
Currently only supports linux-x86_64.

## Install

This is a cargo project. As such, you need to install both the rust toolchain 
and cargo. You should probably use `rustup` to manage rust toolchains 
(See [The Rust Book](https://doc.rust-lang.org/book/ch01-01-installation.html)
for more information).
You'll also need the gnu compiler collection (`gcc`) as the compiler uses the 
gnu linker and assembler for the final binary.

After cloning the repo you can run `cargo build` to build the project.

## Usage

Be sure to specify an input file to compile using the `--file` option.
If you're running the compiler with cargo, you can append cli arguments using
the `--` operator:
```bash
cargo run -- --file main.orca
```

## Basic Syntax

Given the very early stage of this project, syntax is guaranteed to heavily 
change. Current synax looks like this:
```
fn main() {
	let x: usize = 0;
    if (x < 5) {
        x = 10;
    } else {
        let y: usize = 10;
        x = 5 * y;
    }
	return x + 1;
}
```
A basic program can be found in `test/types.orca`. You can run the compiler 
with `cargo run -- --file test/types.orca`. The compiled binary will sit in the
cwd and is called `a.out` by default.
