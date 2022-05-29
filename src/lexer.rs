use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::str::CharIndices;
use Token::*;

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Token<'a> {
    Number(f64),
    Identifier(&'a str),
    Plus,
    Minus,
    Star,
    Slash,
    OpenParen,
    CloseParen,
    Caret,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Error {
    InvalidCharacter(char),
}

struct Lexer<'a> {
    input: &'a str,
    iter: Peekable<CharIndices<'a>>,
    current: char,

    // CharIndices::offset is unstable :'(
    offset: usize,
}

pub fn lex(input: &str) -> impl Iterator<Item = Result<Token, Error>> {
    Lexer {
        input,
        iter: input.char_indices().peekable(),
        current: char::from(0),
        offset: 0,
    }
}

impl<'a> Lexer<'a> {
    fn next_token(&mut self) -> Option<Result<Token<'a>, Error>> {
        loop {
            let c = self.next_char()?;

            return if c.is_whitespace() {
                continue;
            } else if let Some(token) = self.read_symbol() {
                Some(Ok(token))
            } else if let Some(token) = self.read_number() {
                Some(Ok(token))
            } else if let Some(token) = self.read_identifier() {
                Some(Ok(token))
            } else {
                Some(Err(Error::InvalidCharacter(c)))
            };
        }
    }

    fn read_symbol(&self) -> Option<Token<'a>> {
        match self.current {
            '+' => Some(Plus),
            '-' => Some(Minus),
            '*' => Some(Star),
            '/' => Some(Slash),
            '(' => Some(OpenParen),
            ')' => Some(CloseParen),
            '^' => Some(Caret),
            _ => None,
        }
    }

    fn read_number(&mut self) -> Option<Token<'a>> {
        if !self.current.is_ascii_digit() {
            return None;
        }

        let start_offset = self.offset;
        let mut has_period = false;

        loop {
            match self.peek().unwrap_or_default() {
                '0'..='9' => {
                    self.next_char();
                }
                '.' => {
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

    fn read_identifier(&mut self) -> Option<Token<'a>> {
        if !self.current.is_ascii_alphabetic() {
            return None;
        }

        let start_offset = self.offset;

        while self.peek().unwrap_or_default().is_ascii_alphanumeric() {
            self.next_char();
        }

        Some(Identifier(&self.input[start_offset..=self.offset]))
    }

    fn next_char(&mut self) -> Option<char> {
        (self.offset, self.current) = self.iter.next()?;
        Some(self.current)
    }

    fn peek(&mut self) -> Option<char> {
        Some(self.iter.peek()?.1)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Number(value) => write!(f, "{}", value),
            Identifier(name) => write!(f, "{}", name),
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Star => write!(f, "*"),
            Slash => write!(f, "/"),
            OpenParen => write!(f, "("),
            CloseParen => write!(f, ")"),
            Caret => write!(f, "^"),
        }
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
        let tokens = lex("(-1 + 2.5) * 3/4^5 ").collect::<Vec<_>>();

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
                Ok(Caret),
                Ok(Number(5.0)),
            ]
        );
    }

    #[test]
    fn identifiers() {
        let tokens = lex("foo + bar2").collect::<Vec<_>>();

        assert_eq!(
            tokens,
            [Ok(Identifier("foo")), Ok(Plus), Ok(Identifier("bar2")),]
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
