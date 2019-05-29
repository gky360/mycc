#[macro_use]
extern crate log;

mod helpers;

use helpers::{assert_exit_status, run_test};

#[test]
fn step_01_single_literal() {
    run_test(|| {
        assert_exit_status("step_01/single_literal.c", &[], 123);
    });
}

#[test]
fn step_02_single_literal() {
    run_test(|| {
        assert_exit_status("step_02/add_sub.c", &[], 21);
    });
}

#[test]
fn step_03_single_literal() {
    run_test(|| {
        assert_exit_status("step_03/with_whitespace.c", &[], 41);
    });
}

#[test]
fn step_04_basic_arithmetics() {
    run_test(|| {
        assert_exit_status("step_04/basic_arithmetics_01.c", &[], 47);
        assert_exit_status("step_04/basic_arithmetics_02.c", &[], 15);
        assert_exit_status("step_04/basic_arithmetics_03.c", &[], 4);
    });
}

#[test]
fn step_05_unary_operator() {
    run_test(|| {
        assert_exit_status("step_05/unary_operator_01.c", &[], 256 - 3);
        assert_exit_status("step_05/unary_operator_02.c", &[], 256 - 8);
        assert_exit_status("step_05/unary_operator_03.c", &[], 256 - 15);
    });
}
