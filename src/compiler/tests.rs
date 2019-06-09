use super::*;
use std::str::FromStr;

use crate::parser::Ast;

#[test]
fn step_09_syntax_err_bad_lvalue() {
    let source = r##"
int main() {
    int a = 2;
    a + 3 = 4;
}
"##;
    let ast = Ast::from_str(source).unwrap();
    let mut compiler = Compiler::new();
    assert_eq!(
        compiler.compile(&ast),
        Err(CompileError::lval_required(Loc(33, 38)))
    );

    let source = r##"
int main() {
    int a = 2;
    -a = 3;
    return a;
}
"##;
    let ast = Ast::from_str(source).unwrap();
    let mut compiler = Compiler::new();
    assert_eq!(
        compiler.compile(&ast),
        Err(CompileError::lval_required(Loc(33, 35)))
    );
}
