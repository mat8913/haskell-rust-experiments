extern crate gcc;

use std::io::BufReader;
use std::io::BufRead;
use std::fs::read_dir;
use std::fs::File;
use std::path::Path;
use std::process::Command;
use std::process::Stdio;
use std::str;

fn main() {
    for x in link_haskell_package("rust-haskell-hsbits", Path::new("hsbits")) {
        println!("{}", x);
    }
}

fn link_haskell_package(name: &str, path: &Path) -> Vec<String> {
    let mut ret: Vec<String> = Vec::new();

    // Build the package
    let ghc_libdir = call_command(Command::new("cabal")
                                          .arg("exec")
                                          .arg("--")
                                          .arg("ghc")
                                          .arg("--print-libdir")
                                          .current_dir(&path),
                                  "failed to get ghc libdir").stdout;
    let ghc_rtsdir: String = format!("{}/rts", str::from_utf8(&ghc_libdir).unwrap().trim());
    call_command(Command::new("cabal")
                         .arg("exec")
                         .arg("--")
                         .arg("ghc")
                         .arg("-DLIBSUFFIX=\".so\"")
                         .arg("-DWANTEDRTS=\"\"")
                         .arg(format!("-DGHCRTSDIR={:?}", ghc_rtsdir))
                         .arg("mylinker.hs")
                         .current_dir(&path),
                 "failed to compile \"linker\"");
    call_command(Command::new("cabal")
                         .arg("configure")
                         .arg("--ghc-option=-pgml./mylinker")
                         .arg("--enable-shared")
                         .current_dir(&path),
                 "failed to configure haskell package");
    call_command(Command::new("cabal")
                         .arg("build")
                         .current_dir(&path),
                 "failed to build haskell package");

    // Link to the dependencies
    let opts_file   = File::open(path.join("dist").join("cargoOpts")).unwrap();
    let opts_reader = BufReader::new(opts_file);
    for x in opts_reader.lines() {
        ret.push(x.unwrap());
    }

    // Link to the package
    let mut compiler = gcc::Config::new();
    for entry in read_dir(path.join("dist").join("build")).unwrap() {
        let entry = entry.unwrap();
        if let Some(_) = entry.file_name().to_str().and_then(|x| strip_prefix_suffix("", ".dyn_o", x)) {
            compiler.object(entry.path());
        }
    }
    compiler.compile(&format!("libHS{}.a", name));

    return ret;
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
