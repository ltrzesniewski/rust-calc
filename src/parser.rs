use crate::arena::*;
use crate::lexer::{Token, Token::*};
use std::fmt::{Display, Formatter};
use Node::*;

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Node<'a> {
    Value(f64),
    Variable(&'a str),
    Negation(&'a Node<'a>),
    Addition(&'a Node<'a>, &'a Node<'a>),
    Subtraction(&'a Node<'a>, &'a Node<'a>),
    Multiplication(&'a Node<'a>, &'a Node<'a>),
    Division(&'a Node<'a>, &'a Node<'a>),
    Exponentiation(&'a Node<'a>, &'a Node<'a>),
    Function(&'a str, &'a Node<'a>),
}

#[derive(PartialEq, Debug)]
pub enum Error<'a> {
    EmptyStream,
    UnexpectedToken(Token<'a>),
    UnexpectedEndOfStream,
    UnexpectedTrailingToken(Token<'a>),
}

struct Parser<'a, T: Iterator<Item = Token<'a>>> {
    // This is an LL(1) parser
    iter: T,
    current: Option<Token<'a>>,
    next: Option<Token<'a>>,
    arena: &'a Arena<Node<'a>>,
}

type NodeResult<'a> = Result<&'a Node<'a>, Error<'a>>;

pub fn parse<'a>(
    tokens: impl IntoIterator<Item = Token<'a>>,
    arena: &'a Arena<Node<'a>>,
) -> Result<&'a Node<'a>, Error<'a>> {
    let mut parser = Parser::new(tokens.into_iter().fuse(), arena);

    if parser.current == None {
        return Err(Error::EmptyStream);
    }

    let expr = parser.parse_expression()?;

    if let Some(token) = parser.current {
        return Err(Error::UnexpectedTrailingToken(token));
    }

    Ok(expr)
}

impl<'a, T: Iterator<Item = Token<'a>>> Parser<'a, T> {
    fn new(tokens: T, arena: &'a Arena<Node<'a>>) -> Parser<'a, T> {
        let mut iter = tokens;
        let (current, next) = (iter.next(), iter.next());

        Parser {
            iter,
            current,
            next,
            arena,
        }
    }

    fn next_token(&mut self) {
        (self.current, self.next) = (self.next, self.iter.next());
    }

    fn consume_token(&mut self, token: Token) -> Result<(), Error<'a>> {
        return match self.current {
            Some(current) if current == token => {
                self.next_token();
                Ok(())
            }
            Some(other) => Err(Error::UnexpectedToken(other)),
            None => Err(Error::UnexpectedEndOfStream),
        };
    }

    fn parse_expression(&mut self) -> NodeResult<'a> {
        self.parse_terms()
    }

    fn parse_terms(&mut self) -> NodeResult<'a> {
        let mut left = self.parse_factors()?;

        loop {
            let op = match self.current {
                Some(Plus) => Addition,
                Some(Minus) => Subtraction,
                _ => return Ok(left),
            };

            self.next_token();
            let right = self.parse_factors()?;

            left = self.arena.alloc(op(left, right));
        }
    }

    fn parse_factors(&mut self) -> NodeResult<'a> {
        let mut left = self.parse_exponents()?;

        loop {
            let op = match self.current {
                Some(Star) => Multiplication,
                Some(Slash) => Division,
                _ => return Ok(left),
            };

            self.next_token();
            let right = self.parse_exponents()?;

            left = self.arena.alloc(op(left, right))
        }
    }

    fn parse_exponents(&mut self) -> NodeResult<'a> {
        let mut left = self.parse_unary()?;

        loop {
            if self.current != Some(Caret) {
                return Ok(left);
            }

            self.next_token();
            let right = self.parse_exponents()?;

            left = self.arena.alloc(Exponentiation(left, right))
        }
    }

    fn parse_unary(&mut self) -> NodeResult<'a> {
        return match self.current {
            Some(Number(value)) => {
                self.next_token();
                Ok(self.arena.alloc(Value(value)))
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
                let expr = self.parse_unary()?;
                Ok(self.arena.alloc(Negation(expr)))
            }
            Some(Identifier(name)) => {
                self.next_token();
                if self.current == Some(OpenParen) {
                    self.next_token();
                    let expr = self.parse_expression()?;
                    self.consume_token(CloseParen)?;
                    Ok(self.arena.alloc(Function(name, expr)))
                } else {
                    Ok(self.arena.alloc(Variable(name)))
                }
            }
            Some(other) => Err(Error::UnexpectedToken(other)),
            None => Err(Error::UnexpectedEndOfStream),
        };
    }
}

impl Display for Node<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value(value) => write!(f, "{}", value),
            Variable(name) => write!(f, "{}", name),
            Negation(node) => write!(f, "-{}", node),
            Addition(left, right) => write!(f, "({} + {})", left, right),
            Subtraction(left, right) => write!(f, "({} - {})", left, right),
            Multiplication(left, right) => write!(f, "({} * {})", left, right),
            Division(left, right) => write!(f, "({} / {})", left, right),
            Exponentiation(base, exponent) => write!(f, "({}^{})", base, exponent),
            Function(name, arg) => write!(f, "{}({})", name, arg),
        }
    }
}

impl Display for Error<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::EmptyStream => write!(f, "Empty input"),
            Error::UnexpectedToken(token) => write!(f, "Unexpected token: {:?}", token),
            Error::UnexpectedEndOfStream => write!(f, "Unterminated expression"),
            Error::UnexpectedTrailingToken(token) => write!(f, "Extraneous input: {:?}", token),
        }
    }
}

impl std::error::Error for Error<'_> {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[rustfmt::skip]
    fn valid() {
        let arena = Arena::new();

        //  -1 + 2*3 - 4/5^6 + 7
        let result = parse([
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
            Caret,
            Number(6.0),
            Plus,
            Number(7.0),
        ].into_iter(), &arena);

        assert_eq!(
            result.unwrap(),

            arena.alloc(Addition(
                arena.alloc(Subtraction(
                    arena.alloc(Addition(
                        arena.alloc(Negation(
                            arena.alloc(Value(1.0))
                        )),
                        arena.alloc(Multiplication(
                            arena.alloc(Value(2.0)),
                            arena.alloc(Value(3.0)),
                        )),
                    )),
                    arena.alloc(Division(
                        arena.alloc(Value(4.0)),
                        arena.alloc(Exponentiation(
                            arena.alloc(Value(5.0)),
                            arena.alloc(Value(6.0)),
                        )),
                    )),
                )),
                arena.alloc(Value(7.0)),
            ))
        )
    }

    #[test]
    fn exponentiation_is_right_associative() {
        let arena = Arena::new();

        let result = parse(
            [
                Identifier("a"),
                Caret,
                Identifier("b"),
                Caret,
                Identifier("c"),
            ]
            .into_iter(),
            &arena,
        );

        assert_eq!(
            result.unwrap(),
            arena.alloc(Exponentiation(
                arena.alloc(Variable("a")),
                arena.alloc(Exponentiation(
                    arena.alloc(Variable("b")),
                    arena.alloc(Variable("c")),
                ))
            ))
        );
    }

    #[test]
    fn empty() {
        let arena = Arena::new();
        let result = parse([].into_iter(), &arena);
        assert_eq!(result.unwrap_err(), Error::EmptyStream)
    }

    #[test]
    fn trailing() {
        let arena = Arena::new();
        let result = parse([Number(1.0), Number(2.0)].into_iter(), &arena);
        assert_eq!(
            result.unwrap_err(),
            Error::UnexpectedTrailingToken(Number(2.0))
        )
    }

    #[test]
    fn unexpected_eof() {
        let arena = Arena::new();
        let result = parse([Minus].into_iter(), &arena);
        assert_eq!(result.unwrap_err(), Error::UnexpectedEndOfStream)
    }

    #[test]
    fn unexpected_eof_2() {
        let arena = Arena::new();
        let result = parse([Number(1.0), Minus].into_iter(), &arena);
        assert_eq!(result.unwrap_err(), Error::UnexpectedEndOfStream)
    }
}
