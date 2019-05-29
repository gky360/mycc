#[macro_use]
extern crate log;

mod helpers;

#[test]
fn step_01_single_literal() {
    helpers::assert_exit_status("step_01/single_literal.c", &[], 123);
}

#[test]
fn step_02_single_literal() {
    helpers::assert_exit_status("step_02/add_sub.c", &[], 21);
}

#[test]
fn step_03_single_literal() {
    helpers::assert_exit_status("step_03/with_whitespace.c", &[], 41);
}
