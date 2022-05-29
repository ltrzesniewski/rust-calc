use crate::parser::{Node, Node::*};
use crate::{lexer, parser};
use std::cell::Cell;
use std::fmt::{Display, Formatter};
use std::ops::Deref;

pub struct EvalResult<'a> {
    pub ast: Box<Node<'a>>,
    pub evaluated: Box<Node<'a>>,
}

#[derive(PartialEq, Debug)]
pub enum Error<'a> {
    UnknownFunction(&'a str),
}

pub fn eval_str<'a>(input: &'a str) -> Result<EvalResult<'a>, Box<dyn std::error::Error + 'a>> {
    let lexer_error = Cell::new(None);

    let tokens = lexer::lex(input).map_while(|item| match item {
        Ok(token) => Some(token),
        Err(error) => {
            lexer_error.set(Some(error));
            None
        }
    });

    let ast = parser::parse(tokens);

    // Lexer errors should be returned first
    if let Some(error) = lexer_error.get() {
        return Err(Box::new(error));
    }

    let ast = ast?;
    let result = eval(ast.deref())?;

    Ok(EvalResult {
        ast,
        evaluated: Box::new(result),
    })
}

fn eval<'a>(node: &Node<'a>) -> Result<Node<'a>, Error<'a>> {
    return match node {
        Value(value) => Ok(Value(*value)),
        Variable(name) => Ok(eval_var(name)),
        Negation(node) => Ok(match eval(node)? {
            Value(value) => Value(-value),
            other => Negation(Box::new(other)),
        }),
        Addition(left, right) => Ok(match (eval(left)?, eval(right)?) {
            (Value(left), Value(right)) => Value(left + right),
            (left, right) => Addition(Box::new(left), Box::new(right)),
        }),
        Subtraction(left, right) => Ok(match (eval(left)?, eval(right)?) {
            (Value(left), Value(right)) => Value(left - right),
            (left, right) => Subtraction(Box::new(left), Box::new(right)),
        }),
        Multiplication(left, right) => Ok(match (eval(left)?, eval(right)?) {
            (Value(left), Value(right)) => Value(left * right),
            (left, right) => Multiplication(Box::new(left), Box::new(right)),
        }),
        Division(left, right) => Ok(match (eval(left)?, eval(right)?) {
            (Value(left), Value(right)) => Value(left / right),
            (left, right) => Division(Box::new(left), Box::new(right)),
        }),
        Exponentiation(base, exponent) => Ok(match (eval(base)?, eval(exponent)?) {
            (Value(base), Value(exponent)) => Value(f64::powf(base, exponent)),
            (base, exponent) => Exponentiation(Box::new(base), Box::new(exponent)),
        }),
        Function(name, arg) => {
            let func = get_func(name)?;
            let arg = eval(arg)?;
            Ok(if let Value(value) = arg {
                Value(func(value))
            } else {
                Function(name, Box::new(arg))
            })
        }
    };

    fn eval_var(name: &str) -> Node {
        match name {
            "pi" => Value(std::f64::consts::PI),
            "e" => Value(std::f64::consts::E),
            "nan" => Value(f64::NAN), // Why not? :)
            "inf" => Value(f64::INFINITY),
            _ => Variable(name),
        }
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

impl EvalResult<'_> {
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
