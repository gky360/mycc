#[macro_use]
extern crate log;

use std::process::Command;
use tempdir::TempDir;

use helpers::{assert_exit_status, assert_output, run_test, temp_file_name};

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
    run_test(|| {
        // compile foo.c and foo_x_y.c
        let tmp_dir = TempDir::new("mycc").expect("failed to create temp dir");
        for name in &["foo", "foo_x_y"] {
            let output = Command::new("gcc")
                .args(&[
                    "-o",
                    &temp_file_name(&tmp_dir, &format!("{}.o", name)),
                    "-c",
                    &format!("testdata/step_14/{}.c", name),
                ])
                .output()
                .expect("failed to compile func declaration");
            if !output.status.success() {
                eprintln!(
                    "{}",
                    String::from_utf8(output.stderr).expect("failed to output gcc error")
                );
                panic!("gcc exited with failure");
            }
        }

        assert_output(
            "step_14/valid/call_func_01.c",
            &[&temp_file_name(&tmp_dir, "foo.o")],
            &[],
            0,
            "OK\n",
            "",
        );
        assert_output(
            "step_14/valid/call_func_02.c",
            &[&temp_file_name(&tmp_dir, "foo_x_y.o")],
            &[],
            0,
            "OK: 3, 12\n",
            "",
        );
    });
}

#[test]
#[cfg_attr(tarpaulin, skip)]
fn step_15_declare_func() {
    run_test(|| {
        assert_exit_status("step_15/valid/expression_args.c", &[], &[], 14);
        assert_exit_status("step_15/valid/fib.c", &[], &[], 5);
        assert_exit_status("step_15/valid/forward_decl.c", &[], &[], 3);
        assert_exit_status("step_15/valid/forward_decl_args.c", &[], &[], 4);
        assert_exit_status("step_15/valid/forward_decl_multi_arg.c", &[], &[], 256 - 1);
        assert_exit_status("step_15/valid/fun_in_expr.c", &[], &[], 16);
        assert_output(
            "step_15/valid/hello_world.c",
            &[],
            &[],
            0,
            "Hello, World!\n",
            "",
        );
        assert_exit_status("step_15/valid/later_decl.c", &[], &[], 5);
        assert_exit_status("step_15/valid/multi_arg.c", &[], &[], 4);
        assert_exit_status("step_15/valid/mutual_recursion.c", &[], &[], 12);
        assert_exit_status("step_15/valid/no_arg.c", &[], &[], 3);
        assert_exit_status("step_15/valid/precedence.c", &[], &[], 256 - 3);
        assert_exit_status("step_15/valid/rename_function_param.c", &[], &[], 4);
        assert_exit_status("step_15/valid/single_arg.c", &[], &[], 6);
        assert_exit_status("step_15/valid/variable_as_arg.c", &[], &[], 2);
    });
}

#[test]
#[cfg_attr(tarpaulin, skip)]
fn step_17_pointer() {
    run_test(|| {
        assert_exit_status("step_17/valid/pointer_01.c", &[], &[], 3);
        assert_exit_status("step_17/valid/pointer_02.c", &[], &[], 3);
    });
}

#[test]
#[cfg_attr(tarpaulin, skip)]
fn step_18_pointer_arithmetic() {
    run_test(|| {
        // compile helper source code
        let tmp_dir = TempDir::new("mycc").expect("failed to create temp dir");
        let output = Command::new("gcc")
            .args(&[
                "-o",
                &temp_file_name(&tmp_dir, "alloc4.o"),
                "-c",
                "testdata/step_18/alloc4.c",
            ])
            .output()
            .expect("failed to compile func declaration");
        if !output.status.success() {
            eprintln!(
                "{}",
                String::from_utf8(output.stderr).expect("failed to output gcc error")
            );
            panic!("gcc exited with failure");
        }

        assert_exit_status(
            "step_18/valid/pointer_arithmetic_01.c",
            &[&temp_file_name(&tmp_dir, "alloc4.o")],
            &[],
            8,
        );
    });
}

#[test]
#[cfg_attr(tarpaulin, skip)]
fn step_19_sizeof() {
    run_test(|| {
        assert_exit_status("step_19/valid/sizeof_int_const.c", &[], &[], 4);
        assert_exit_status("step_19/valid/sizeof_int_expr_1.c", &[], &[], 4);
        assert_exit_status("step_19/valid/sizeof_int_expr_2.c", &[], &[], 4);
        assert_exit_status("step_19/valid/sizeof_int_pointer_expr.c", &[], &[], 8);
        assert_exit_status("step_19/valid/sizeof_int_pointer_var.c", &[], &[], 8);
        assert_exit_status("step_19/valid/sizeof_int_var.c", &[], &[], 4);
        assert_exit_status("step_19/valid/sizeof_sizeof.c", &[], &[], 4);
    });
}

#[test]
#[cfg_attr(tarpaulin, skip)]
fn step_20_array() {
    run_test(|| {
        assert_exit_status("step_20/valid/2d_array_arg.c", &[], &[], 7);
        assert_exit_status("step_20/valid/2d_array_pointer_1.c", &[], &[], 9);
        assert_exit_status("step_20/valid/2d_array_pointer_2.c", &[], &[], 7);
        assert_exit_status("step_20/valid/2d_sizeof.c", &[], &[], 4 * 3 * 3);
        assert_exit_status("step_20/valid/array_arg.c", &[], &[], 11);
        assert_exit_status("step_20/valid/declare_array.c", &[], &[], 0);
        assert_exit_status("step_20/valid/deref_assign.c", &[], &[], 5);
        assert_exit_status("step_20/valid/expr_index.c", &[], &[], 5);
        assert_exit_status("step_20/valid/index_access_1.c", &[], &[], 0);
        assert_exit_status("step_20/valid/index_access_2.c", &[], &[], 7);
        assert_exit_status("step_20/valid/pointer_access.c", &[], &[], 0);
        assert_exit_status("step_20/valid/sizeof_array.c", &[], &[], 4 * 5);
    });
}

#[test]
#[cfg_attr(tarpaulin, skip)]
fn step_22_global_var() {
    run_test(|| {
        assert_exit_status("step_20/valid/declare_global_var.c", &[], &[], 7);
        assert_exit_status("step_20/valid/global_arr.c", &[], &[], 7);
        assert_exit_status("step_20/valid/init.c", &[], &[], 9);
        assert_exit_status("step_20/valid/shadow_1.c", &[], &[], 3);
        assert_exit_status("step_20/valid/shadow_2.c", &[], &[], 5);
    });
}
