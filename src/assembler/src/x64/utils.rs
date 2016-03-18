use std::process::{Stdio, Command};
use std::path::Path;
use std::io::{BufReader, BufRead, Write};
use std::fs::File;
use tempdir::TempDir;

pub fn dump_file<A: AsRef<Path>>(path: A, bs: &[u8]) {
    let mut f = File::create(path).unwrap();
    f.write_all(bs).unwrap();
}

pub fn objdump_disas_lines(bs: &[u8]) -> Vec<String> {

    let tdir = TempDir::new("assembler.x64.text").unwrap();
    let mut tmp_path = tdir.path().to_owned();
    tmp_path.push("disassembly");

    dump_file(&tmp_path, bs);

    let mut objdump = Command::new("objdump")
        .arg("-D")
        .arg("-bbinary")
        .arg("-mi386")
        .arg("-Mx86-64")
        .arg(&tmp_path)
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();

    let stdout = BufReader::new(objdump.stdout.as_mut().unwrap());
    stdout.lines().skip(7 /* Non-disassembly lines */).map(|line| line.unwrap()).collect()
}
