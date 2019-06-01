use super::*;
use std::str::FromStr;

use crate::parser::Ast;

#[test]
fn test_step_09_syntax_err_bad_lvalue() {
    let source = r##"
a = 2;
a + 3 = 4;
"##;
    let ast = Ast::from_str(source).unwrap();
    let mut compiler = Compiler::new();
    assert_eq!(
        compiler.compile(&ast),
        Err(CompileError::lval_required(Loc(8, 13)))
    );
}