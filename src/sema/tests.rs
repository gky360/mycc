use super::*;
use std::str::FromStr;

use crate::sema::analyze;
use crate::tests::load_source;
use crate::Error;

fn assert_sema_error(name: &str) {
    let source = load_source(name);
    let mut ast = Ast::from_str(&source).unwrap();
    match analyze(&mut ast) {
        Ok(_) => assert!(false, "sema returned no error: {}", name),
        Err(err) => {
            err.show_diagnostic(&source);
            Error::from(err).show_trace();
            assert!(true);
        }
    }
}

#[test]
fn step_09_syntax_err_bad_lvalue() {
    assert_sema_error("step_09/sema_err/invalid_lval_1.c");
    assert_sema_error("step_09/sema_err/invalid_lval_2.c");
}

#[test]
fn step_18_pointer_arithmetic() {
    assert_sema_error("step_18/sema_err/add_pointers.c");
    assert_sema_error("step_18/sema_err/nonpointer_deref.c");
}
