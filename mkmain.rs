#![feature(exit_status)]
use std::env;
use std::io::Write;
use std::fs::File;

static TEMPLATE: &'static str = "\
#![feature(exit_status)]
extern crate @UTIL_CRATE@ as uu@UTIL_CRATE@;

use std::env;
use uu@UTIL_CRATE@::uumain;

fn main() {
    env::set_exit_status(uumain(env::args().collect()));
}
";

fn main() {
    let args : Vec<String> = env::args().collect();
    if args.len() != 3 {
        println!("usage: mkbuild <crate> <outfile>");
        env::set_exit_status(1);
        return;
    }

    let crat    = &args[1][..];
    let outfile = &args[2][..];

    let main = TEMPLATE.replace("@UTIL_CRATE@", crat);
    match File::create(outfile) {
        Ok(mut out) => match out.write_all(main.as_bytes()) {
            Err(e) => panic!("{}", e),
            _ => (),
        },
        Err(e) => panic!("{}", e),
    }
}
