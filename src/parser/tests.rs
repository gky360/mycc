use std::fs;
use std::path::PathBuf;

use super::*;

fn testdata_path(name: &str) -> PathBuf {
    PathBuf::from("testdata").join(name)
}

fn load_source(name: &str) -> std::io::Result<String> {
    fs::read_to_string(testdata_path(name))
}

fn assert_parse_error(name: &str) {
    let source = load_source(name).expect("failed to load test source file");
    match Ast::from_str(&source) {
        Ok(_) => assert!(false, "parser returned no error"),
        Err(ParseError::Lex(_)) => assert!(false, "lexer returned error"),
        Err(_) => assert!(true),
    }
}

#[test]
fn step_06_relational_operator() {
    let names = vec![
        "step_06/invalid/missing_first_op.c",
        "step_06/invalid/missing_mid_op.c",
        "step_06/invalid/missing_second_op.c",
    ];
    names.iter().for_each(|name| assert_parse_error(name))
}

#[test]
fn step_09_single_char_variable() {
    let names = vec!["step_09/invalid/syntax_err_no_semicolon.c"];
    names.iter().for_each(|name| assert_parse_error(name))
}

#[test]
fn step_12_1_control_flow() {
    let names = vec![
        "step_12_1/invalid/if_assignment.c",
        "step_12_1/invalid/mismatched_nesting.c",
    ];
    names.iter().for_each(|name| assert_parse_error(name))
}

#[test]
fn step_12_2_control_flow() {
    let names = vec![
        "step_12_2/invalid/syntax_err_empty_clause.c",
        "step_12_2/invalid/syntax_err_paren_mismatch.c",
        "step_12_2/invalid/syntax_err_too_few_for_clauses.c",
        "step_12_2/invalid/syntax_err_too_many_for_clauses.c",
    ];
    names.iter().for_each(|name| assert_parse_error(name))
}

#[test]
fn step_13_compound_statement() {
    let names = vec!["step_13/invalid/compound_statement_01.c"];
    names.iter().for_each(|name| assert_parse_error(name))
}

#[test]
fn step_13_call_func() {
    let names = vec!["step_14/invalid/call_func_01.c"];
    names.iter().for_each(|name| assert_parse_error(name))
}
