use crate::lexer::{Token, Token::*};
use Node::*;

#[derive(PartialEq, Debug)]
pub enum Node {
    Value(f64),
    Negation(Box<Node>),
    Addition(Box<Node>, Box<Node>),
    Subtraction(Box<Node>, Box<Node>),
    Multiplication(Box<Node>, Box<Node>),
    Division(Box<Node>, Box<Node>),
}

#[derive(PartialEq, Debug)]
pub enum Error {
    EmptyStream,
    UnexpectedToken(Token),
    UnexpectedEndOfStream,
    UnexpectedTrailingToken(Token),
}

pub struct Parser<T: Iterator<Item = Token>> {
    // This is an LL(1) parser
    iter: T,
    current: Option<Token>,
    next: Option<Token>,
}

type ParseResult = Result<Box<Node>, Error>;

impl<T: Iterator<Item = Token>> Parser<T> {
    // TODO : Try to make the following signature work: parse(tokens: impl IntoIterator<Item=Token>)
    pub fn parse(tokens: T) -> ParseResult {
        let mut parser = Parser::new(tokens.into_iter().fuse());

        if parser.current == None {
            return Err(Error::EmptyStream);
        }

        let expr = parser.parse_expression()?;

        if let Some(token) = parser.current {
            return Err(Error::UnexpectedTrailingToken(token));
        }

        Ok(expr)
    }

    fn new(tokens: T) -> Parser<T> {
        let mut iter = tokens;
        let (current, next) = (iter.next(), iter.next());

        Parser {
            iter,
            current,
            next,
        }
    }

    fn next_token(&mut self) {
        (self.current, self.next) = (self.next, self.iter.next());
    }

    fn consume_token(&mut self, token: Token) -> Result<(), Error> {
        return match self.current {
            Some(current) if current == token => {
                self.next_token();
                Ok(())
            }
            Some(other) => Err(Error::UnexpectedToken(other)),
            None => Err(Error::UnexpectedEndOfStream),
        };
    }

    fn parse_expression(&mut self) -> ParseResult {
        self.parse_terms()
    }

    fn parse_terms(&mut self) -> ParseResult {
        let mut left = self.parse_factors()?;

        loop {
            let op = match self.current {
                Some(Plus) => Addition,
                Some(Minus) => Subtraction,
                _ => return Ok(left),
            };

            self.next_token();
            let right = self.parse_factors()?;

            left = Box::new(op(left, right));
        }
    }

    fn parse_factors(&mut self) -> ParseResult {
        let mut left = self.parse_unary()?;

        loop {
            let op = match self.current {
                Some(Star) => Multiplication,
                Some(Slash) => Division,
                _ => return Ok(left),
            };

            self.next_token();
            let right = self.parse_unary()?;

            left = Box::new(op(left, right))
        }
    }

    fn parse_unary(&mut self) -> ParseResult {
        return match self.current {
            Some(Number(value)) => {
                self.next_token();
                Ok(Box::new(Value(value)))
            }
            Some(OpenParen) => {
                self.next_token();
                let expr = self.parse_expression()?;
                self.consume_token(CloseParen)?;
                Ok(expr)
            }
            Some(Plus) => {
                self.next_token();
                self.parse_unary()
            }
            Some(Minus) => {
                self.next_token();
                Ok(Box::new(Negation(self.parse_unary()?)))
            }
            Some(other) => Err(Error::UnexpectedToken(other)),
            None => Err(Error::UnexpectedEndOfStream),
        };
    }
}

#[cfg(test)]
#[rustfmt::skip]
mod tests {
    use super::*;

    #[test]
    fn valid() {
        //  -1 + 2*3 - 4/5 + 6
        let result = Parser::parse([
            Minus,
            Number(1.0),
            Plus,
            Number(2.0),
            Star,
            Number(3.0),
            Minus,
            Number(4.0),
            Slash,
            Number(5.0),
            Plus,
            Number(6.0),
        ].into_iter());

        assert_eq!(
            result,
            Ok(
                new(Addition(
                    new(Subtraction(
                        new(Addition(
                            new(Negation(
                                new(Value(1.0))
                            )),
                            new(Multiplication(
                                new(Value(2.0)),
                                new(Value(3.0)),
                            )),
                        )),
                        new(Division(
                            new(Value(4.0)),
                            new(Value(5.0)),
                        )),
                    )),
                    new(Value(6.0)),
                ))
            )
        )
    }

    #[test]
    fn empty() {
        let result = Parser::parse([].into_iter());

        assert_eq!(result, Err(Error::EmptyStream))
    }

    #[test]
    fn trailing() {
        let result = Parser::parse([Number(1.0), Number(2.0)].into_iter());

        assert_eq!(result, Err(Error::UnexpectedTrailingToken(Number(2.0))))
    }

    #[test]
    fn unexpected_eof() {
        let result = Parser::parse([Minus].into_iter());

        assert_eq!(result, Err(Error::UnexpectedEndOfStream))
    }

    #[test]
    fn unexpected_eof_2() {
        let result = Parser::parse([Number(1.0), Minus].into_iter());

        assert_eq!(result, Err(Error::UnexpectedEndOfStream))
    }

    fn new(value: Node) -> Box<Node> {
        Box::new(value)
    }
}
