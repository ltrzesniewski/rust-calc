use crate::parser;
use std::fmt::{Display, Formatter};
use Node::*;

// Ok, this is overkill, but the goal here is to write code after all :)

#[derive(PartialEq, Debug)]
pub enum Node {
    Value(f64),
    Negation(Box<Node>),
    Addition(Box<Node>, Box<Node>),
    Subtraction(Box<Node>, Box<Node>),
    Multiplication(Box<Node>, Box<Node>),
    Division(Box<Node>, Box<Node>),
    Function(fn(f64) -> f64, Box<Node>),
    Function2(fn(f64, f64) -> f64, Box<Node>, Box<Node>),
}

#[derive(PartialEq, Debug)]
pub enum Error<'a> {
    UnknownConstant(&'a str),
    UnknownFunction(&'a str),
}

type ParserNode<'a> = parser::Node<'a>;

pub fn bind<'a>(node: &ParserNode<'a>) -> Result<Box<Node>, Error<'a>> {
    match node {
        ParserNode::Value(value) => Ok(Box::new(Value(*value))),
        ParserNode::Constant(name) => Ok(Box::new(Value(bind_constant(name)?))),
        ParserNode::Negation(node) => Ok(Box::new(Negation(bind(node)?))),
        ParserNode::Addition(left, right) => Ok(Box::new(Addition(bind(left)?, bind(right)?))),
        ParserNode::Subtraction(left, right) => {
            Ok(Box::new(Subtraction(bind(left)?, bind(right)?)))
        }
        ParserNode::Multiplication(left, right) => {
            Ok(Box::new(Multiplication(bind(left)?, bind(right)?)))
        }
        ParserNode::Division(left, right) => Ok(Box::new(Division(bind(left)?, bind(right)?))),
        ParserNode::Exponentiation(base, exponent) => {
            Ok(Box::new(Function2(f64::powf, bind(base)?, bind(exponent)?)))
        }
        ParserNode::Function(name, node) => {
            Ok(Box::new(Function(bind_function(name)?, bind(node)?)))
        }
    }
}

fn bind_constant(name: &str) -> Result<f64, Error> {
    match name {
        "pi" => Ok(std::f64::consts::PI),
        "e" => Ok(std::f64::consts::E),
        "nan" => Ok(f64::NAN), // Why not? :)
        "inf" => Ok(f64::INFINITY),
        _ => Err(Error::UnknownConstant(name)),
    }
}

fn bind_function(name: &str) -> Result<fn(f64) -> f64, Error> {
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

impl Display for Error<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UnknownConstant(name) => write!(f, "Unknown constant: {}", name),
            Error::UnknownFunction(name) => write!(f, "Unknown function: {}", name),
        }
    }
}

impl std::error::Error for Error<'_> {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn constant() {
        assert_eq!(bind_constant("pi"), Ok(std::f64::consts::PI)); // Meh.
        assert_eq!(bind_constant("lol"), Err(Error::UnknownConstant("lol")));
    }

    #[test]
    fn function() {
        assert!(bind_function("sin").eq(&Ok(f64::sin)));
        assert_eq!(bind_function("lol"), Err(Error::UnknownFunction("lol")));
    }
}
