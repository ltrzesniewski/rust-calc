use crate::binder::{Node, Node::*};
use crate::{binder, lexer, parser};
use std::cell::Cell;
use std::error::Error;
use std::ops::Deref;

pub fn eval_str<'a>(input: &'a str) -> Result<f64, Box<dyn Error + 'a>> {
    let lexer_error = Cell::new(None);

    let tokens = lexer::lex(input).map_while(|item| match item {
        Ok(token) => Some(token),
        Err(error) => {
            lexer_error.set(Some(error));
            None
        }
    });

    let result = parser::parse(tokens);

    // Lexer errors should be returned first
    if let Some(error) = lexer_error.get() {
        return Err(Box::new(error));
    }

    let result = binder::bind(*result?)?;
    Ok(eval_node(result.deref()))
}

fn eval_node(node: &Node) -> f64 {
    return match node {
        Value(value) => *value,
        Negation(node) => -eval_node(node),
        Addition(left, right) => eval_node(left) + eval_node(right),
        Subtraction(left, right) => eval_node(left) - eval_node(right),
        Multiplication(left, right) => eval_node(left) * eval_node(right),
        Division(left, right) => eval_node(left) / eval_node(right),
        Function(func, node) => func(eval_node(node)),
        Function2(func, first, second) => func(eval_node(first), eval_node(second)),
    };
}
