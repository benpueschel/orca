use std::io::Result;
use std::process::Command;

use crate::target::TargetPlatform;

pub fn assemble(target: TargetPlatform, input_file: &str, output_file: &str) -> Result<()> {
    let process = match target {
        TargetPlatform::LinuxX86_64 => Command::new("as")
            .args(["-o", output_file, input_file])
            .output()?,
    };

    if process.status.success() {
        println!(
            "{}",
            String::from_utf8(process.stdout).expect("could not parse stdout")
        );
    } else {
        println!(
            "{}",
            String::from_utf8(process.stderr).expect("could not parse stderr")
        );
    };
    Ok(())
}
