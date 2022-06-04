use atty::Stream;
use rust_calc::*;
use std::io;
use std::io::Write;

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
            Ok(result) => {
                println!("= {}", result.ast);

                #[cfg(debug_assertions)]
                println!("= {}", result.intermediate);

                println!("= {}", result.result);
            }
            Err(error) => eprintln!("{}", error),
        }
    }

    Ok(())
}
