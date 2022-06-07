use rust_calc::*;

fn get_str_result(expr: &str) -> String {
    eval_str(expr).unwrap().result.to_string()
}

#[test]
fn it_calculates() {
    assert_eq!(eval_str("2+2").unwrap().value(), Some(4.0));
    assert_eq!(eval_str("1 + 2 * 3").unwrap().value(), Some(7.0));
}

#[test]
fn it_returns_error() {
    assert!(eval_str("µ").is_err());
    assert!(eval_str("2+").is_err());
    assert!(eval_str("foo bar").is_err());
}

#[test]
fn it_handles_unknown_symbols() {
    assert_eq!(
        get_str_result("2+2+foo(bar+baz+6/2)"),
        "(4 + foo(((bar + baz) + 3)))"
    );
}

#[test]
fn it_differentiates() {
    assert_eq!(get_str_result("diff(x^2 + cos(x))"), "((2 * x) - sin(x))");
    assert_eq!(get_str_result("diff(2*foo + x*bar)"), "bar");
}

#[test]
fn it_differentiates_unknown_functions() {
    assert_eq!(
        get_str_result("diff(x + foo(x^2))"),
        "(1 + diff(foo((x^2))))"
    );
}
