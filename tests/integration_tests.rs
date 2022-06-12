use rust_calc::*;

fn get_str_result(expr: &str) -> String {
    let arena = Arena::new();
    eval_str(expr, &arena).unwrap().result.to_string()
}

#[test]
fn it_calculates() {
    let arena = Arena::new();
    assert_eq!(eval_str("2+2", &arena).unwrap().value(), Some(4.0));
    assert_eq!(eval_str("1 + 2 * 3", &arena).unwrap().value(), Some(7.0));
}

#[test]
fn it_returns_error() {
    let arena = Arena::new();
    assert!(eval_str("µ", &arena).is_err());
    assert!(eval_str("2+", &arena).is_err());
    assert!(eval_str("foo bar", &arena).is_err());
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
