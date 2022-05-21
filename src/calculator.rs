use crate::parser::{Node, Node::*};

pub fn evaluate(node: &Node) -> f64 {
    return match node {
        Value(value) => *value,
        Negation(value) => -evaluate(value),
        Addition(left, right) => evaluate(left) + evaluate(right),
        Subtraction(left, right) => evaluate(left) - evaluate(right),
        Multiplication(left, right) => evaluate(left) * evaluate(right),
        Division(left, right) => evaluate(left) / evaluate(right),
    };
}
