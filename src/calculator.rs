use crate::parser::{Node, Node::*};
use crate::{lexer, parser};
use std::error::Error;
use std::ops::Deref;

pub fn eval_str(input: &str) -> Result<f64, Box<dyn Error>> {
    let mut lexer_error = None;

    let tokens = lexer::lex(input).map_while(|item| match item {
        Ok(token) => Some(token),
        Err(error) => {
            lexer_error = Some(error);
            None
        }
    });

    let result = parser::parse(tokens);

    if let Some(error) = lexer_error {
        return Err(Box::new(error));
    }

    match result {
        Ok(ast) => Ok(eval_node(ast.deref())),
        Err(error) => Err(Box::new(error)),
    }
}

fn eval_node(node: &Node) -> f64 {
    return match node {
        Value(value) => *value,
        Negation(value) => -eval_node(value),
        Addition(left, right) => eval_node(left) + eval_node(right),
        Subtraction(left, right) => eval_node(left) - eval_node(right),
        Multiplication(left, right) => eval_node(left) * eval_node(right),
        Division(left, right) => eval_node(left) / eval_node(right),
    };
}
