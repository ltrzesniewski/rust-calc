use atty::Stream;
use std::io;
use std::io::Write;
use rust_calc::*;

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

        match eval_str(input) {
            Ok(result) => println!("= {}", result),
            Err(error) => eprintln!("{}", error),
        }
    }

    Ok(())
}
