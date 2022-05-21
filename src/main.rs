mod calculator;
mod lexer;
mod parser;

use atty::Stream;
use std::error::Error;
use std::io;
use std::io::Write;
use std::ops::Deref;

fn main() -> io::Result<()> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut input = String::new();

    loop {
        if atty::is(Stream::Stdout) {
            print!("> ");
            stdout.flush().unwrap();
        }

        input.clear();
        stdin.read_line(&mut input)?;
        let input = input.trim();

        if input.is_empty() {
            continue;
        }

        if input == "exit" {
            break;
        }

        match eval(input) {
            Ok(result) => println!("= {}", result),
            Err(error) => eprintln!("{}", error),
        }
    }

    Ok(())
}

fn eval(input: &str) -> Result<f64, Box<dyn Error>> {
    let mut tokens = Vec::new();

    for item in lexer::lex(input) {
        match item {
            Ok(token) => tokens.push(token),
            Err(error) => return Err(Box::new(error)),
        }
    }

    match parser::parse(tokens.into_iter()) {
        Ok(ast) => Ok(calculator::evaluate(ast.deref())),
        Err(error) => Err(Box::new(error)),
    }
}
