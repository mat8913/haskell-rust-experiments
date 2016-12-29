extern crate gcc;

use std::fs::read_dir;
use std::fs::File;
use std::path::Path;
use std::process::Command;
use std::process::Stdio;
use std::str;
use std::io;

fn main() {
    link_haskell_package("rust-haskell-hsbits", Path::new("hsbits"));
}

fn link_haskell_package(name: &str, path: &Path) {
    // Build the package
    call_command(Command::new("cabal")
                         .arg("build")
                         .current_dir(&path),
                 "failed to build haskell package");

    // Link to the dependencies
    let mut opts_file = File::open(path.join("dist").join("build").join("cargoOpts")).unwrap();
    io::copy(&mut opts_file, &mut io::stdout()).unwrap();

    // Link to the package
    let mut compiler = gcc::Config::new();
    for entry in read_dir(path.join("dist").join("build")).unwrap() {
        let entry = entry.unwrap();
        if let Some(_) = entry.file_name().to_str().and_then(|x| strip_prefix_suffix("", ".dyn_o", x)) {
            compiler.object(entry.path());
        }
    }
    compiler.compile(&format!("libHS{}.a", name));
}

fn strip_prefix_suffix<'a>(prefix : &str, suffix : &str, string : &'a str) -> Option<&'a str> {
    if string.starts_with(prefix) && string.ends_with(suffix) {
        Some(&string[prefix.len() .. string.len() - suffix.len()])
    } else {
        None
    }
}

fn call_command(cmd: &mut Command, failmsg: &str) -> std::process::Output {
    let output = cmd.stderr(Stdio::inherit()).output().expect(failmsg);
    if output.status.success() {
        return output;
    } else {
        panic!(String::from(failmsg));
    }
}
