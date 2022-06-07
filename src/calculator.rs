use crate::arena::Arena;
use crate::parser::{Node, Node::*};
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
pub enum Error {
    NotImplemented(&'static str),
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

    let arena = Arena::new();
    let ast = parser::parse(tokens, &arena);

    // Lexer errors should be returned first
    if let Some(error) = lexer_error.get() {
        return Err(Box::new(error));
    }

    let ast = ast?;
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

    fn eval(&self, node: &Node<'input, 'arena>) -> Result<Node<'input, 'arena>, Error> {
        let a = |n| self.alloc(n);
        let eval = |n| self.eval(n);

        Ok(match node {
            Value(value) => Value(*value),
            Variable(name) => Self::eval_var(name),
            Negation(node) => match eval(node)? {
                Value(value) => Value(-value),
                Negation(node) => *node,
                node => Negation(a(node)),
            },
            Addition(left, right) => match (eval(left)?, eval(right)?) {
                (node, Value(zero)) | (Value(zero), node) if zero == 0.0 => node,
                (Value(left), Value(right)) => Value(left + right),

                (Addition(node_a, Value(val_a)), Addition(node_b, Value(val_b)))
                | (Addition(Value(val_a), node_a), Addition(node_b, Value(val_b)))
                | (Addition(node_a, Value(val_a)), Addition(Value(val_b), node_b))
                | (Addition(Value(val_a), node_a), Addition(Value(val_b), node_b)) => {
                    Addition(a(Addition(node_a, node_b)), a(Value(val_a + val_b)))
                }

                (Addition(inner, Value(ref val_a)), Value(ref val_b))
                | (Addition(Value(ref val_a), inner), Value(ref val_b))
                | (Value(ref val_a), Addition(inner, Value(ref val_b)))
                | (Value(ref val_a), Addition(Value(ref val_b), inner)) => {
                    Addition(inner, a(Value(*val_a + *val_b)))
                }

                (left, right) => Addition(a(left), a(right)),
            },
            Subtraction(left, right) => match (eval(left)?, eval(right)?) {
                (node, Value(zero)) if zero == 0.0 => node,
                (Value(left), Value(right)) => Value(left - right),
                (left, right) => eval(&Addition(a(left), a(Negation(a(right)))))?,
            },
            Multiplication(left, right) => match (eval(left)?, eval(right)?) {
                (_, Value(zero)) | (Value(zero), _) if zero == 0.0 => Value(0.0),
                (node, Value(one)) | (Value(one), node) if one == 1.0 => node,
                (Value(left), Value(right)) => Value(left * right),
                (left, right) => Multiplication(a(left), a(right)),
            },
            Division(left, right) => match (eval(left)?, eval(right)?) {
                (Value(zero), Value(zero2)) if zero == 0.0 && zero2 == 0.0 => Value(f64::NAN),
                (Value(zero), _) if zero == 0.0 => Value(0.0),
                (node, Value(one)) if one == 1.0 => node,
                (Value(left), Value(right)) => Value(left / right),
                (left, right) => Division(a(left), a(right)),
            },
            Exponentiation(base, exponent) => match (eval(base)?, eval(exponent)?) {
                (_, Value(zero)) if zero == 0.0 => Value(1.0),
                (node, Value(one)) if one == 1.0 => node,
                (Value(base), Value(exponent)) => Value(f64::powf(base, exponent)),
                (base, exponent) => Exponentiation(a(base), a(exponent)),
            },
            Function(name, arg) => match *name {
                "diff" => eval(&self.differentiate(arg)?)?,
                "__diff" => Function("diff", a(eval(arg)?)),
                _ => {
                    if let Some(func) = Self::get_func(name) {
                        let arg = eval(arg)?;
                        if let Value(value) = arg {
                            Value(func(value))
                        } else {
                            Function(name, a(arg))
                        }
                    } else {
                        Function(name, a(eval(arg)?))
                    }
                }
            },
        })
    }

    fn prettify(&self, node: &Node<'input, 'arena>) -> &'arena Node<'input, 'arena> {
        let a = |n| self.alloc(n);
        let p = |n| self.prettify(n);

        a(match node {
            // Real rules
            Addition(left, Value(right)) if *right < 0.0 => Subtraction(p(left), a(Value(-*right))),
            Addition(left, Negation(right)) => Subtraction(p(left), p(right)),

            // Forwarding
            Value(_) | Variable(_) => *node,
            Negation(inner) => Negation(p(inner)),
            Addition(left, right) => Addition(p(left), p(right)),
            Subtraction(left, right) => Subtraction(p(left), p(right)),
            Multiplication(left, right) => Multiplication(p(left), p(right)),
            Division(left, right) => Division(p(left), p(right)),
            Exponentiation(left, right) => Exponentiation(p(left), p(right)),
            Function(name, arg) => Function(name, p(arg)),
        })
    }

    fn eval_var(name: &str) -> Node {
        Value(match name {
            "pi" => std::f64::consts::PI,
            "e" => std::f64::consts::E,
            "nan" => f64::NAN, // Why not? :)
            "inf" => f64::INFINITY,
            _ => return Variable(name),
        })
    }

    fn get_func(name: &str) -> Option<fn(f64) -> f64> {
        Some(match name {
            "round" => f64::round,
            "floor" => f64::floor,
            "ceil" => f64::ceil,
            "abs" => f64::abs,
            "sqrt" => f64::sqrt,
            "ln" => f64::ln,
            "log2" => f64::log2,
            "log10" => f64::log10,
            "sin" => f64::sin,
            "cos" => f64::cos,
            "tan" => f64::tan,
            "asin" => f64::asin,
            "acos" => f64::acos,
            "atan" => f64::atan,
            "sinh" => f64::sinh,
            "cosh" => f64::cosh,
            "tanh" => f64::tanh,
            "asinh" => f64::asinh,
            "acosh" => f64::acosh,
            "atanh" => f64::atanh,
            _ => return None,
        })
    }

    fn differentiate(
        &self,
        node: &'arena Node<'input, 'arena>,
    ) -> Result<Node<'input, 'arena>, Error> {
        let a = |n| self.alloc(n);

        macro_rules! d {
            ($node:expr) => {
                self.alloc(self.differentiate($node)?)
            };
        }

        Ok(match node {
            Value(_) => Value(0.0),
            Variable(name) => {
                if *name == "x" {
                    Value(1.0)
                } else {
                    Value(0.0)
                }
            }
            Negation(inner) => Negation(d!(inner)),
            Addition(left, right) => Addition(d!(left), d!(right)),
            Subtraction(left, right) => Subtraction(d!(left), d!(right)),
            Multiplication(left, right) => Addition(
                a(Multiplication(d!(left), right)),
                a(Multiplication(left, d!(right))),
            ),
            Division(left, right) => Division(
                a(Subtraction(
                    a(Multiplication(d!(left), right)),
                    a(Multiplication(left, d!(right))),
                )),
                a(Exponentiation(right, a(Value(2.0)))),
            ),
            Exponentiation(base, Value(exponent)) => Multiplication(
                a(Multiplication(
                    a(Value(*exponent)),
                    a(Exponentiation(base, a(Value(exponent - 1.0)))),
                )),
                d!(base),
            ),
            Function("ln", arg) => Division(d!(arg), arg),
            Function("sqrt", arg) => Division(
                d!(arg),
                a(Multiplication(a(Value(2.0)), a(Function("sqrt", arg)))),
            ),
            Function("sin", arg) => Multiplication(d!(arg), a(Function("cos", arg))),
            Function("cos", arg) => Multiplication(d!(arg), a(Negation(a(Function("sin", arg))))),
            Function("diff", arg) => self.differentiate(d!(arg))?,
            Function(_, _) => Function("__diff", node), // Avoid infinite recursion
            _ => {
                return Err(Error::NotImplemented(
                    "Could not differentiate this expression",
                ));
            }
        })
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

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::NotImplemented(msg) => write!(f, "Not implemented: {}", msg),
        }
    }
}

impl std::error::Error for Error {}
