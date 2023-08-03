use std::io;

use clap::Parser;

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
    let mut buffer = String::new();
    loop {
        print!("> ");
        let res = stdin.read_line(&mut buffer);
        if let Err(e) = res {
            println!("");
            panic!("Error reading input line: {}", e);
        }

//        interpret(buffer);
    }
}

fn run_file(path: String) {
    let src = match std::fs::read_to_string(&path) {
        Ok(s) => s,
        Err(e) => panic!("Error reading source file at: {}, error: {}", path, e),
    };
}
