use std::str::CharIndices;

#[derive(PartialEq, Debug)]
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

pub struct Lexer<'a> {
    input: &'a str,
    iter: CharIndices<'a>,
    current: char,

    // CharIndices::offset is unstable :'(
    offset: usize,
}

impl<'a> Lexer<'a> {
    pub fn lex(input: &'a str) -> Lexer<'a> {
        Lexer {
            input,
            iter: input.char_indices(),
            current: char::from(0),
            offset: 0,
        }
    }

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
            '+' => Some(Token::Plus),
            '-' => Some(Token::Minus),
            '*' => Some(Token::Star),
            '/' => Some(Token::Slash),
            '(' => Some(Token::OpenParen),
            ')' => Some(Token::CloseParen),
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

        Some(Token::Number(
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

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod tests {
    use super::Token::*;
    use super::*;

    #[test]
    fn valid() {
        let tokens = Lexer::lex("(-1 + 2.5) * 3/4 ").collect::<Vec<_>>();

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
        let tokens = Lexer::lex("1.2.3µ").collect::<Vec<_>>();

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
