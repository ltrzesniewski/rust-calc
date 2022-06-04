use crate::arena::Arena;
use crate::parser::{Node, Node::*, ParseResult};
use crate::{lexer, parser};
use std::cell::Cell;
use std::fmt::{Display, Formatter};

pub struct EvalResult<'input, 'arena> {
    pub ast: &'arena Node<'input, 'arena>,
    pub intermediate: &'arena Node<'input, 'arena>,
    pub result: &'arena Node<'input, 'arena>,
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

    let ParseResult { ast, arena } = parse_result?;

    let calc = Calc::new(&arena);

    let intermediate = arena.alloc(calc.eval(ast)?);
    let result = calc.prettify(intermediate);

    Ok(EvalResult {
        ast,
        intermediate,
        result,
        arena,
    })
}

impl<'calc, 'input, 'arena> Calc<'calc, 'input, 'arena> {
    fn new(arena: &'calc Arena<'arena, Node<'input, 'arena>>) -> Calc<'calc, 'input, 'arena> {
        Calc { arena }
    }

    fn eval(&self, node: &Node<'input, 'arena>) -> Result<Node<'input, 'arena>, Error<'input>> {
        let alloc = |n| self.alloc(n);
        let eval = |n| self.eval(n);

        Ok(match node {
            Value(value) => Value(*value),
            Variable(name) => Self::eval_var(name),
            Negation(node) => match eval(node)? {
                Value(value) => Value(-value),
                Negation(node) => *node,
                node => Negation(alloc(node)),
            },
            Addition(left, right) => match (eval(left)?, eval(right)?) {
                (Value(left), Value(right)) => Value(left + right),

                (Addition(node_a, Value(val_a)), Addition(node_b, Value(val_b)))
                | (Addition(Value(val_a), node_a), Addition(node_b, Value(val_b)))
                | (Addition(node_a, Value(val_a)), Addition(Value(val_b), node_b))
                | (Addition(Value(val_a), node_a), Addition(Value(val_b), node_b)) => {
                    Addition(alloc(Addition(node_a, node_b)), alloc(Value(val_a + val_b)))
                }

                (Addition(inner, Value(ref a)), Value(ref b))
                | (Addition(Value(ref a), inner), Value(ref b))
                | (Value(ref a), Addition(inner, Value(ref b)))
                | (Value(ref a), Addition(Value(ref b), inner)) => {
                    Addition(inner, alloc(Value(*a + *b)))
                }

                (left, right) => Addition(alloc(left), alloc(right)),
            },
            Subtraction(left, right) => match (eval(left)?, eval(right)?) {
                (Value(left), Value(right)) => Value(left - right),
                (left, right) => eval(&Addition(alloc(left), alloc(Negation(alloc(right)))))?,
            },
            Multiplication(left, right) => match (eval(left)?, eval(right)?) {
                (Value(left), Value(right)) => Value(left * right),
                (left, right) => Multiplication(alloc(left), alloc(right)),
            },
            Division(left, right) => match (eval(left)?, eval(right)?) {
                (Value(left), Value(right)) => Value(left / right),
                (left, right) => Division(alloc(left), alloc(right)),
            },
            Exponentiation(base, exponent) => match (eval(base)?, eval(exponent)?) {
                (Value(base), Value(exponent)) => Value(f64::powf(base, exponent)),
                (base, exponent) => Exponentiation(alloc(base), alloc(exponent)),
            },
            Function(name, arg) => {
                let func = Self::get_func(name)?;
                let arg = eval(arg)?;
                if let Value(value) = arg {
                    Value(func(value))
                } else {
                    Function(name, alloc(arg))
                }
            }
        })
    }

    fn prettify(&self, node: &Node<'input, 'arena>) -> &'arena Node<'input, 'arena> {
        let alloc = |n| self.alloc(n);

        alloc(match node {
            // Real rules
            Addition(left, Value(right)) if *right < 0.0 => {
                Subtraction(self.prettify(left), alloc(Value(-*right)))
            }

            Addition(left, Negation(right)) => {
                Subtraction(self.prettify(left), self.prettify(right))
            }

            // Forwarding
            Value(_) | Variable(_) => *node,

            Negation(inner) => Negation(self.prettify(inner)),

            Addition(left, right) => Addition(self.prettify(left), self.prettify(right)),

            Subtraction(left, right) => Subtraction(self.prettify(left), self.prettify(right)),

            Multiplication(left, right) => {
                Multiplication(self.prettify(left), self.prettify(right))
            }

            Division(left, right) => Division(self.prettify(left), self.prettify(right)),

            Exponentiation(left, right) => {
                Exponentiation(self.prettify(left), self.prettify(right))
            }

            Function(name, arg) => Function(name, self.prettify(arg)),
        })
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
        if let Value(value) = self.result {
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
