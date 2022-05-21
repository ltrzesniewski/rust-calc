use std::fmt::{Display, Formatter};
use std::str::CharIndices;
use Token::*;

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Token {
    Number(f64),
    Plus,
    Minus,
    Star,
    Slash,
    OpenParen,
    CloseParen,
}

#[derive(PartialEq, Debug)]
pub enum Error {
    InvalidCharacter(char),
}

struct Lexer<'a> {
    input: &'a str,
    iter: CharIndices<'a>,
    current: char,

    // CharIndices::offset is unstable :'(
    offset: usize,
}

pub fn lex(input: &str) -> impl Iterator<Item = Result<Token, Error>> + '_ {
    Lexer {
        input,
        iter: input.char_indices(),
        current: char::from(0),
        offset: 0,
    }
}

impl Lexer<'_> {
    fn next_token(&mut self) -> Option<Result<Token, Error>> {
        loop {
            let c = self.next_char()?;

            return if c.is_whitespace() {
                continue;
            } else if let Some(token) = self.read_symbol() {
                Some(Ok(token))
            } else if let Some(token) = self.read_number() {
                Some(Ok(token))
            } else {
                Some(Err(Error::InvalidCharacter(c)))
            };
        }
    }

    fn read_symbol(&self) -> Option<Token> {
        match self.current {
            '+' => Some(Plus),
            '-' => Some(Minus),
            '*' => Some(Star),
            '/' => Some(Slash),
            '(' => Some(OpenParen),
            ')' => Some(CloseParen),
            _ => None,
        }
    }

    fn read_number(&mut self) -> Option<Token> {
        if !self.current.is_ascii_digit() {
            return None;
        }

        let start_offset = self.offset;
        let mut has_period = false;

        loop {
            match self.peek_u8().unwrap_or_default() {
                b'0'..=b'9' => {
                    self.next_char();
                }
                b'.' => {
                    if has_period {
                        break;
                    }

                    has_period = true;
                    self.next_char();
                }
                _ => break,
            }
        }

        Some(Number(
            self.input[start_offset..=self.offset]
                .parse::<f64>()
                .unwrap(),
        ))
    }

    fn next_char(&mut self) -> Option<char> {
        (self.offset, self.current) = self.iter.next()?;
        Some(self.current)
    }

    fn peek_u8(&self) -> Option<u8> {
        Some(*self.iter.as_str().as_bytes().first()?)
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::InvalidCharacter(char) => write!(f, "Invalid character: {}", char),
        }
    }
}

impl std::error::Error for Error {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn valid() {
        let tokens = lex("(-1 + 2.5) * 3/4 ").collect::<Vec<_>>();

        assert_eq!(
            tokens,
            [
                Ok(OpenParen),
                Ok(Minus),
                Ok(Number(1.0)),
                Ok(Plus),
                Ok(Number(2.5)),
                Ok(CloseParen),
                Ok(Star),
                Ok(Number(3.0)),
                Ok(Slash),
                Ok(Number(4.0)),
            ]
        );
    }

    #[test]
    fn invalid() {
        let tokens = lex("1.2.3µ").collect::<Vec<_>>();

        assert_eq!(
            tokens,
            [
                Ok(Number(1.2)),
                Err(Error::InvalidCharacter('.')),
                Ok(Number(3.0)),
                Err(Error::InvalidCharacter('µ')),
            ]
        );
    }
}
