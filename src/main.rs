mod parse;
mod tokenize;
use parse::{parse, Serialize};
use tokenize::tokenize;

use crate::parse::enumerate_idents;

fn main() {
    use std::{env, io::Read};
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        eprintln!("Usage: {} [infile] [outfile]", args[0]);
        std::process::exit(1);
    }
    let mut in_file = std::fs::File::open(&args[1]).unwrap();
    let mut input = Vec::new();
    in_file.read_to_end(&mut input).unwrap();
    let input = String::from_utf8(input).unwrap();
    let input = input.trim().as_bytes();

    let toks = tokenize(input);
    println!("{:?}", toks);
    let ast = parse(&toks);
    println!("============ AST ============");
    println!("{:#?}", ast);
    let east = enumerate_idents::<u16>(ast);
    println!("============ enumerated AST ============");
    println!("{:#?}", east);
    let mut out_file = std::fs::File::create(&args[2]).unwrap();
    east.serialize(&mut out_file).unwrap();
}
