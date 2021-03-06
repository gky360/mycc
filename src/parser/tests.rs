use super::*;
use crate::tests::load_source;
use crate::Error;

fn assert_parse_error(name: &str) {
    let source = load_source(name);
    match Ast::from_str(&source) {
        Ok(_) => assert!(false, "parser returned no error: {}", name),
        Err(err) => {
            err.show_diagnostic(&source);
            if let ParseError::Lex(_) = err {
                assert!(false, "lexer returned error: {}", name);
            }
            Error::from(err).show_trace();
            assert!(true);
        }
    }
}

#[test]
fn step_06_relational_operator() {
    let names = &[
        "step_06/invalid/missing_first_op.c",
        "step_06/invalid/missing_mid_op.c",
        "step_06/invalid/missing_second_op.c",
    ];
    names.iter().for_each(|name| assert_parse_error(name))
}

#[test]
fn step_09_single_char_variable() {
    let names = &["step_09/parse_err/syntax_err_no_semicolon.c"];
    names.iter().for_each(|name| assert_parse_error(name))
}

#[test]
fn step_12_1_control_flow() {
    let names = &[
        "step_12_1/invalid/if_assignment.c",
        "step_12_1/invalid/mismatched_nesting.c",
    ];
    names.iter().for_each(|name| assert_parse_error(name))
}

#[test]
fn step_12_2_control_flow() {
    let names = &[
        "step_12_2/invalid/syntax_err_empty_clause.c",
        "step_12_2/invalid/syntax_err_paren_mismatch.c",
        "step_12_2/invalid/syntax_err_too_few_for_clauses.c",
        "step_12_2/invalid/syntax_err_too_many_for_clauses.c",
    ];
    names.iter().for_each(|name| assert_parse_error(name))
}

#[test]
fn step_13_compound_statement() {
    let names = &["step_13/invalid/compound_statement_01.c"];
    names.iter().for_each(|name| assert_parse_error(name))
}

#[test]
fn step_14_call_func() {
    let names = &["step_14/invalid/call_func_01.c"];
    names.iter().for_each(|name| assert_parse_error(name))
}

#[test]
fn step_15_declare_func() {
    let names: &[&str] = &[
        // TODO: comment in these tests
        // "step_15/invalid/bad_arg.c",
        "step_15/invalid/conflict_types.c",
        // "step_15/invalid/declaration_mismatch.c",
        // "step_15/invalid/declaration_mismatch_2.c",
        // "step_15/invalid/redefine_function.c",
        // "step_15/invalid/too_many_args.c",
    ];
    names.iter().for_each(|name| assert_parse_error(name))
}

#[test]
fn step_16_explicit_declare() {
    let names = &[
        "step_16/invalid/double_define.c",
        "step_16/invalid/if_assignment.c",
        "step_16/invalid/redefine.c",
        "step_16/invalid/syntax_err_bad_decl.c",
        "step_16/invalid/syntax_err_bad_decl_2.c",
        "step_16/invalid/undeclared_var.c",
        "step_16/invalid/var_declared_late.c",
    ];
    names.iter().for_each(|name| assert_parse_error(name))
}

#[test]
fn step_20_array() {
    assert_parse_error("step_20/parse_err/variable_length.c");
}

#[test]
fn step_22_global_var() {
    assert_parse_error("step_22/parse_err/global_arr_variable_len.c");
    assert_parse_error("step_22/parse_err/undeclared.c");
}
