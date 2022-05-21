mod lexer;

use lexer::Lexer;

fn main() {
    for item in Lexer::lex("1 + 2 * 3") {
        match item {
            Ok(token) => println!("Token: {:?}", token),
            Err(error) => eprintln!("Error: {:?}", error),
        }
    }
}
