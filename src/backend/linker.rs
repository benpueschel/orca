use std::io::Result;
use std::process::Command;

use crate::target::TargetPlatform;

pub fn link(target: TargetPlatform, input_file: &str, output_file: &str) -> Result<()> {
    let process = match target {
        TargetPlatform::LinuxX86_64 => Command::new("ld")
            .args([
                "-o",
                output_file,
                "-dynamic-linker",
                "/lib64/ld-linux-x86-64.so.2",
                "/usr/lib/crt1.o",
                "/usr/lib/crti.o",
                "-lc",
                input_file,
                "/usr/lib/crtn.o",
            ])
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
