use crate::arena::Arena;
use crate::parser::{Node, Node::*};
use crate::{lexer, parser};
use std::cell::Cell;
use std::fmt::{Display, Formatter};
use std::ops::Deref;

pub struct EvalResult<'input, 'arena> {
    pub ast: &'arena Node<'input, 'arena>,
    pub evaluated: &'arena Node<'input, 'arena>,
    pub arena: Arena<'arena, Node<'input, 'arena>>,
}

#[derive(PartialEq, Debug)]
pub enum Error<'input> {
    UnknownFunction(&'input str),
}

struct Calc<'calc, 'input, 'arena> {
    arena: &'calc Arena<'arena, Node<'input, 'arena>>,
}

pub fn eval_str<'input, 'arena>(
    input: &'input str,
) -> Result<EvalResult<'input, 'arena>, Box<dyn std::error::Error + 'input>> {
    let lexer_error = Cell::new(None);

    let tokens = lexer::lex(input).map_while(|item| match item {
        Ok(token) => Some(token),
        Err(error) => {
            lexer_error.set(Some(error));
            None
        }
    });

    let parse_result = parser::parse(tokens);

    // Lexer errors should be returned first
    if let Some(error) = lexer_error.get() {
        return Err(Box::new(error));
    }

    let parse_result = parse_result?;

    let calc = Calc::new(&parse_result.arena);
    let result = calc.eval(parse_result.ast.deref())?;
    let result = parse_result.arena.alloc(result);

    Ok(EvalResult {
        ast: parse_result.ast,
        evaluated: result,
        arena: parse_result.arena,
    })
}

impl<'calc, 'input, 'arena> Calc<'calc, 'input, 'arena> {
    fn new(arena: &'calc Arena<'arena, Node<'input, 'arena>>) -> Calc<'calc, 'input, 'arena> {
        Calc { arena }
    }

    fn eval(&self, node: &Node<'input, 'arena>) -> Result<Node<'input, 'arena>, Error<'input>> {
        return match node {
            Value(value) => Ok(Value(*value)),
            Variable(name) => Ok(Self::eval_var(name)),
            Negation(node) => Ok(match self.eval(node)? {
                Value(value) => Value(-value),
                other => Negation(self.alloc(other)),
            }),
            Addition(left, right) => Ok(match (self.eval(left)?, self.eval(right)?) {
                (Value(left), Value(right)) => Value(left + right),
                // // Keep constants on the right side
                // (Addition(left, right), Value(value)) | (Value(value), Addition(left, right)) => {
                //     match (*left, *right) {
                //         (Value(value2), other) | (other, Value(value2)) => {
                //             Addition(self.alloc(other), self.alloc(Value(value + value2)))
                //         }
                //         (left, right) => Addition(
                //             self.alloc(Addition(self.alloc(left), self.alloc(right))),
                //             self.alloc(Value(value)),
                //         ),
                //     }
                // }
                // (Addition(a, b), Addition(c, d)) => match (*a, *b, *c, *d) {
                //     (a, Value(b), c, Value(d)) => Addition(
                //         self.alloc(Addition(self.alloc(a), self.alloc(c))),
                //         self.alloc(Value(b + d)),
                //     ),
                //     (a, b, c, d) => Addition(
                //         self.alloc(Addition(self.alloc(a), self.alloc(b))),
                //         self.alloc(Addition(self.alloc(c), self.alloc(d))),
                //     ),
                // },
                // (Value(left), right) => Addition(self.alloc(right), self.alloc(Value(left))),
                (left, right) => Addition(self.alloc(left), self.alloc(right)),
            }),
            Subtraction(left, right) => Ok(match (self.eval(left)?, self.eval(right)?) {
                (Value(left), Value(right)) => Value(left - right),
                (left, right) => Subtraction(self.alloc(left), self.alloc(right)),
            }),
            Multiplication(left, right) => Ok(match (self.eval(left)?, self.eval(right)?) {
                (Value(left), Value(right)) => Value(left * right),
                (left, right) => Multiplication(self.alloc(left), self.alloc(right)),
            }),
            Division(left, right) => Ok(match (self.eval(left)?, self.eval(right)?) {
                (Value(left), Value(right)) => Value(left / right),
                (left, right) => Division(self.alloc(left), self.alloc(right)),
            }),
            Exponentiation(base, exponent) => Ok(match (self.eval(base)?, self.eval(exponent)?) {
                (Value(base), Value(exponent)) => Value(f64::powf(base, exponent)),
                (base, exponent) => Exponentiation(self.alloc(base), self.alloc(exponent)),
            }),
            Function(name, arg) => {
                let func = Self::get_func(name)?;
                let arg = self.eval(arg)?;
                Ok(if let Value(value) = arg {
                    Value(func(value))
                } else {
                    Function(name, self.alloc(arg))
                })
            }
        };
    }
    fn eval_var(name: &str) -> Node {
        match name {
            "pi" => Value(std::f64::consts::PI),
            "e" => Value(std::f64::consts::E),
            "nan" => Value(f64::NAN), // Why not? :)
            "inf" => Value(f64::INFINITY),
            _ => Variable(name),
        }
    }
    fn get_func(name: &str) -> Result<fn(f64) -> f64, Error> {
        match name {
            "round" => Ok(f64::round),
            "floor" => Ok(f64::floor),
            "ceil" => Ok(f64::ceil),
            "abs" => Ok(f64::abs),
            "sqrt" => Ok(f64::sqrt),
            "ln" => Ok(f64::ln),
            "log2" => Ok(f64::log2),
            "log10" => Ok(f64::log10),
            "sin" => Ok(f64::sin),
            "cos" => Ok(f64::cos),
            "tan" => Ok(f64::tan),
            "asin" => Ok(f64::asin),
            "acos" => Ok(f64::acos),
            "atan" => Ok(f64::atan),
            "sinh" => Ok(f64::sinh),
            "cosh" => Ok(f64::cosh),
            "tanh" => Ok(f64::tanh),
            "asinh" => Ok(f64::asinh),
            "acosh" => Ok(f64::acosh),
            "atanh" => Ok(f64::atanh),
            _ => Err(Error::UnknownFunction(name)),
        }
    }

    fn alloc(&self, node: Node<'input, 'arena>) -> &'arena Node<'input, 'arena> {
        self.arena.alloc(node)
    }
}

impl EvalResult<'_, '_> {
    pub fn value(&self) -> Option<f64> {
        if let Value(value) = self.evaluated.deref() {
            Some(*value)
        } else {
            None
        }
    }
}

impl Display for Error<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UnknownFunction(name) => write!(f, "Unknown function: {}", name),
        }
    }
}

impl std::error::Error for Error<'_> {}
