#[macro_use]
extern crate log;

use std::ffi::OsStr;
use std::process::Command;

use helpers::{assert_exit_status, assert_output, run_test};

#[cfg_attr(tarpaulin, skip)]
mod helpers;

#[test]
#[cfg_attr(tarpaulin, skip)]
fn step_01_single_literal() {
    run_test(|| {
        assert_exit_status("step_01/single_literal.c", &[], &[], 123);
    });
}

#[test]
#[cfg_attr(tarpaulin, skip)]
fn step_02_single_literal() {
    run_test(|| {
        assert_exit_status("step_02/add_sub.c", &[], &[], 21);
    });
}

#[test]
#[cfg_attr(tarpaulin, skip)]
fn step_03_single_literal() {
    run_test(|| {
        assert_exit_status("step_03/with_whitespace.c", &[], &[], 41);
    });
}

#[test]
#[cfg_attr(tarpaulin, skip)]
fn step_04_basic_arithmetics() {
    run_test(|| {
        assert_exit_status("step_04/basic_arithmetics_01.c", &[], &[], 47);
        assert_exit_status("step_04/basic_arithmetics_02.c", &[], &[], 15);
        assert_exit_status("step_04/basic_arithmetics_03.c", &[], &[], 4);
    });
}

#[test]
#[cfg_attr(tarpaulin, skip)]
fn step_05_unary_operator() {
    run_test(|| {
        assert_exit_status("step_05/unary_operator_01.c", &[], &[], 256 - 3);
        assert_exit_status("step_05/unary_operator_02.c", &[], &[], 256 - 8);
        assert_exit_status("step_05/unary_operator_03.c", &[], &[], 256 - 15);
    });
}

#[test]
#[cfg_attr(tarpaulin, skip)]
fn step_06_relational_operator() {
    run_test(|| {
        assert_exit_status("step_06/valid/relational_operator_01.c", &[], &[], 1);
        assert_exit_status("step_06/valid/relational_operator_02.c", &[], &[], 1);
        assert_exit_status("step_06/valid/relational_operator_03.c", &[], &[], 1);
        assert_exit_status("step_06/valid/relational_operator_04.c", &[], &[], 1);
        assert_exit_status("step_06/valid/relational_operator_05.c", &[], &[], 0);
    });
}

#[test]
#[cfg_attr(tarpaulin, skip)]
fn step_09_single_char_varibale() {
    run_test(|| {
        assert_exit_status("step_09/valid/assign_val.c", &[], &[], 0);
        assert_exit_status("step_09/valid/assign.c", &[], &[], 2);
        assert_exit_status("step_09/valid/multiple_vars.c", &[], &[], 3);
        assert_exit_status("step_09/valid/no_initialize.c", &[], &[], 0);
        assert_exit_status("step_09/valid/refer.c", &[], &[], 2);
        assert_exit_status("step_09/valid/unused_exp.c", &[], &[], 0);

        assert_exit_status("step_09/valid/single_char_variable_01.c", &[], &[], 13);
    });
}

#[test]
#[cfg_attr(tarpaulin, skip)]
fn step_10_return() {
    run_test(|| {
        assert_exit_status("step_10/valid/return_01.c", &[], &[], 14);
        assert_exit_status("step_10/valid/return_02.c", &[], &[], 5);
    });
}

#[test]
#[cfg_attr(tarpaulin, skip)]
fn step_11_multi_chars_variable() {
    run_test(|| {
        assert_exit_status("step_11/valid/multi_chars_variable_01.c", &[], &[], 6);
        assert_exit_status("step_11/valid/multi_chars_variable_02.c", &[], &[], 6);
        assert_exit_status("step_11/valid/multi_chars_variable_03.c", &[], &[], 1);
        assert_exit_status("step_11/valid/multi_chars_variable_04.c", &[], &[], 1);
    });
}

#[test]
#[cfg_attr(tarpaulin, skip)]
fn step_12_1_control_flow() {
    run_test(|| {
        assert_exit_status("step_12_1/valid/else.c", &[], &[], 2);
        assert_exit_status("step_12_1/valid/if_nested_1.c", &[], &[], 1);
        assert_exit_status("step_12_1/valid/if_nested_2.c", &[], &[], 2);
        assert_exit_status("step_12_1/valid/if_nested_3.c", &[], &[], 3);
        assert_exit_status("step_12_1/valid/if_nested_4.c", &[], &[], 4);
        assert_exit_status("step_12_1/valid/if_nested_5.c", &[], &[], 1);
        assert_exit_status("step_12_1/valid/if_not_taken.c", &[], &[], 0);
        assert_exit_status("step_12_1/valid/if_taken.c", &[], &[], 1);
        assert_exit_status("step_12_1/valid/multiple_if.c", &[], &[], 8);
    });
}

#[test]
#[cfg_attr(tarpaulin, skip)]
fn step_12_2_control_flow() {
    run_test(|| {
        assert_exit_status("step_12_2/valid/for_decl.c", &[], &[], 3);
        assert_exit_status("step_12_2/valid/for.c", &[], &[], 3);
        assert_exit_status("step_12_2/valid/return_in_while.c", &[], &[], 2);
        assert_exit_status("step_12_2/valid/while_single_statement.c", &[], &[], 6);
    });
}

#[test]
#[cfg_attr(tarpaulin, skip)]
fn step_13_compound_statement() {
    run_test(|| {
        assert_exit_status("step_13/valid/return_in_while.c", &[], &[], 2);
        assert_exit_status("step_13/valid/while_multi_statement.c", &[], &[], 6);
    });
}

#[test]
#[cfg_attr(tarpaulin, skip)]
fn step_14_call_func() {
    for name in &["foo", "foo_x_y"] {
        Command::new("gcc")
            .args(&[
                "-o",
                &format!("testdata/step_14/{}.o", name),
                "-c",
                &format!("testdata/step_14/{}.c", name),
            ])
            .output()
            .expect("failed to compile func declaration");
    }

    run_test(|| {
        assert_output(
            "step_14/valid/call_func_01.c",
            &[OsStr::new("testdata/step_14/foo.o")],
            &[],
            0,
            "OK\n",
            "",
        );
        assert_output(
            "step_14/valid/call_func_02.c",
            &[OsStr::new("testdata/step_14/foo_x_y.o")],
            &[],
            0,
            "OK: 3, 12\n",
            "",
        );
    });
}
