use crate::arena::*;
use crate::lexer::{Token, Token::*};
use std::fmt::{Display, Formatter};
use Node::*;

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum Node<'input, 'arena> {
    Value(f64),
    Variable(&'input str),
    Negation(&'arena Node<'input, 'arena>),
    Addition(&'arena Node<'input, 'arena>, &'arena Node<'input, 'arena>),
    Subtraction(&'arena Node<'input, 'arena>, &'arena Node<'input, 'arena>),
    Multiplication(&'arena Node<'input, 'arena>, &'arena Node<'input, 'arena>),
    Division(&'arena Node<'input, 'arena>, &'arena Node<'input, 'arena>),
    Exponentiation(&'arena Node<'input, 'arena>, &'arena Node<'input, 'arena>),
    Function(&'input str, &'arena Node<'input, 'arena>),
}

#[derive(PartialEq, Debug)]
pub enum Error<'input> {
    EmptyStream,
    UnexpectedToken(Token<'input>),
    UnexpectedEndOfStream,
    UnexpectedTrailingToken(Token<'input>),
}

struct Parser<'parser, 'input, 'arena, T: Iterator<Item = Token<'input>>> {
    // This is an LL(1) parser
    iter: T,
    current: Option<Token<'input>>,
    next: Option<Token<'input>>,
    arena: &'parser Arena<'arena, Node<'input, 'arena>>,
}

type NodeResult<'input, 'arena> = Result<&'arena Node<'input, 'arena>, Error<'input>>;

pub fn parse<'input, 'arena>(
    tokens: impl IntoIterator<Item = Token<'input>>,
    arena: &Arena<'arena, Node<'input, 'arena>>,
) -> Result<&'arena Node<'input, 'arena>, Error<'input>> {
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

impl<'parser, 'input, 'arena, T: Iterator<Item = Token<'input>>>
    Parser<'parser, 'input, 'arena, T>
{
    fn new(
        tokens: T,
        arena: &'parser Arena<'arena, Node<'input, 'arena>>,
    ) -> Parser<'parser, 'input, 'arena, T> {
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

    fn consume_token(&mut self, token: Token) -> Result<(), Error<'input>> {
        return match self.current {
            Some(current) if current == token => {
                self.next_token();
                Ok(())
            }
            Some(other) => Err(Error::UnexpectedToken(other)),
            None => Err(Error::UnexpectedEndOfStream),
        };
    }

    fn parse_expression(&mut self) -> NodeResult<'input, 'arena> {
        self.parse_terms()
    }

    fn parse_terms(&mut self) -> NodeResult<'input, 'arena> {
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

    fn parse_factors(&mut self) -> NodeResult<'input, 'arena> {
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

    fn parse_exponents(&mut self) -> NodeResult<'input, 'arena> {
        let mut left = self.parse_unary()?;

        loop {
            if self.current != Some(Caret) {
                return Ok(left);
            }

            self.next_token();
            let right = self.parse_unary()?;

            left = self.arena.alloc(Exponentiation(left, right))
        }
    }

    fn parse_unary(&mut self) -> NodeResult<'input, 'arena> {
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

impl Display for Node<'_, '_> {
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
        let  arena = Arena::new();

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
    fn empty() {
        let result = parse([].into_iter(), &Arena::new());
        assert_eq!(result.unwrap_err(), Error::EmptyStream)
    }

    #[test]
    fn trailing() {
        let result = parse([Number(1.0), Number(2.0)].into_iter(), &Arena::new());
        assert_eq!(
            result.unwrap_err(),
            Error::UnexpectedTrailingToken(Number(2.0))
        )
    }

    #[test]
    fn unexpected_eof() {
        let result = parse([Minus].into_iter(), &Arena::new());
        assert_eq!(result.unwrap_err(), Error::UnexpectedEndOfStream)
    }

    #[test]
    fn unexpected_eof_2() {
        let result = parse([Number(1.0), Minus].into_iter(), &Arena::new());
        assert_eq!(result.unwrap_err(), Error::UnexpectedEndOfStream)
    }
}
