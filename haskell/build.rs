use std::path::Path;
use std::process::Command;
use std::process::Stdio;
use std::env;

fn main() {
    link_haskell_package("rust-haskell-hsbits", Path::new("hsbits"));
}

fn link_haskell_package(name: &str, path: &Path) {
    let builddir = Path::new(&env::var("OUT_DIR").unwrap()).join(format!("HS{}-dist", name));
    call_command(Command::new("cabal")
                         .arg("build")
                         .arg("--builddir")
                         .arg(builddir.to_str().unwrap())
                         .current_dir(&path)
                         .stdout(Stdio::inherit()),
                 "failed to build haskell package");
}

fn call_command(cmd: &mut Command, failmsg: &str) -> std::process::Output {
    let output = cmd.stderr(Stdio::inherit()).output().expect(failmsg);
    if output.status.success() {
        return output;
    } else {
        panic!(String::from(failmsg));
    }
}
