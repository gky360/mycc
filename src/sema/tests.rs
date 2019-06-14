use super::*;
use std::str::FromStr;

use crate::sema::{analyze, SemanticError};

#[test]
fn step_09_syntax_err_bad_lvalue() {
    let source = r##"
int main() {
    int a = 2;
    a + 3 = 4;
}
"##;
    let mut ast = Ast::from_str(source).unwrap();
    assert_eq!(
        analyze(&mut ast),
        Err(SemanticError::lval_required(&Loc(33, 38)))
    );

    let source = r##"
int main() {
    int a = 2;
    -a = 3;
    return a;
}
"##;
    let mut ast = Ast::from_str(source).unwrap();
    assert_eq!(
        analyze(&mut ast),
        Err(SemanticError::lval_required(&Loc(33, 35)))
    );
}
