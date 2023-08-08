use std::io::{self, Write};

use clap::Parser;

use rlox::run;

#[derive(Parser)]
#[command(author, about)]
struct CliArgs {
    path: Option<String>,
}

fn main() {
    let args = CliArgs::parse();
    match args.path {
        None => repl(),
        Some(path) => run_file(path),
    }
}

fn repl() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut buffer = String::new();
    loop {
        print!("> ");
        stdout.flush().expect("error flushing stdout");
        let res = stdin.read_line(&mut buffer);
        if let Err(e) = res {
            println!("");
            panic!("Error reading input line: {}", e);
        }
        match run::run_program(&buffer) {
            Ok(val) => println!("{val}"),
            Err(e) => println!("{e:?}"),
        }
    }
}

fn run_file(path: String) {
    let src = match std::fs::read_to_string(&path) {
        Ok(s) => s,
        Err(e) => panic!("Error reading source file at: {}, error: {}", path, e),
    };
}
