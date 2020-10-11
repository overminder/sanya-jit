use std::process::{Stdio, Command};
use std::path::Path;
use std::io::{BufReader, BufRead, Write};
use std::fs::File;
use tempdir::TempDir;

pub fn dump_file<A: AsRef<Path>>(path: A, bs: &[u8]) {
    let mut f = File::create(path).unwrap();
    f.write_all(bs).unwrap();
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn objdump_path() -> &'static str {
    if cfg!(target_os = "linux") {
        "objdump"
    } else if cfg!(target_os = "macos") {
        // E.g. installed by brew
        "/usr/local/opt/binutils/bin/gobjdump"
    } else {
        panic!("Should never happen");
    }
}

#[cfg(any(target_os = "linux", target_os = "macos"))]
fn objdump_disas_lines_internal(obj_path: &Path) -> Vec<String> {
    let mut objdump = Command::new(objdump_path())
        .arg("-D")
        .arg("-bbinary")
        .arg("-mi386")
        .arg("-Mx86-64")
        .arg(obj_path)
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();

    let stdout = BufReader::new(objdump.stdout.as_mut().unwrap());
    stdout.lines().skip(7 /* Non-disassembly lines */).map(|line| line.unwrap()).collect()
}

pub fn objdump_disas_lines(bs: &[u8]) -> Vec<String> {
    let tdir = TempDir::new("assembler.x64.text").unwrap();
    let mut tmp_path = tdir.path().to_owned();
    tmp_path.push("disassembly");

    dump_file(&tmp_path, bs);
    objdump_disas_lines_internal(&tmp_path)
}
