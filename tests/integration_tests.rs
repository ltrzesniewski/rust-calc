use rust_calc::*;

#[test]
fn it_calculates() {
    assert_eq!(eval_str("2+2").unwrap().value, 4.0);
    assert_eq!(eval_str("1 + 2 * 3").unwrap().value, 7.0);
}

#[test]
fn it_returns_error() {
    assert!(eval_str("µ").is_err());
    assert!(eval_str("2+").is_err());
    assert!(eval_str("foo bar").is_err());
    assert!(eval_str("foo(42)").is_err());
}
