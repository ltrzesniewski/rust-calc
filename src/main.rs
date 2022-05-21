mod lexer;
mod parser;
mod calculator;

use lexer::Lexer;
use parser::Parser;
use atty::Stream;
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
            Ok(result) => println!("{result}"),
            Err(error) => eprintln!("{error}"),
        }
    }

    Ok(())
}

fn eval(input: &str) -> Result<f64, String> {
    let mut tokens = Vec::new();

    for item in Lexer::lex(input) {
        match item {
            Ok(token) => tokens.push(token),
            Err(lexer::Error::InvalidCharacter(char)) => {
                return Err(format!("Invalid character: {}", char));
            }
        }
    }

    match Parser::parse(tokens.into_iter()) {
        Ok(ast) => Ok(calculator::evaluate(ast.deref())),
        Err(parser::Error::EmptyStream) => Err(String::from("Empty input")),
        Err(parser::Error::UnexpectedToken(token)) => Err(format!("Unexpected token: {:?}", token)),
        Err(parser::Error::UnexpectedEndOfStream) => Err(String::from("Unterminated expression")),
        Err(parser::Error::UnexpectedTrailingToken(token)) => {
            Err(format!("Extraneous input: {:?}", token))
        }
    }
}
