use std::fmt;

use crate::lexer::{Annot, Loc};
use crate::parser::{Ast, AstNode, BinOp, BinOpKind, Type, UniOp, UniOpKind};

pub type Result<T> = std::result::Result<T, SemanticError>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SemanticErrorKind {}

#[derive(Fail, Debug, Clone, PartialEq, Eq, Hash)]
pub struct SemanticError(Annot<SemanticErrorKind>);

impl SemanticError {
    fn new(kind: SemanticErrorKind, loc: Loc) -> Self {
        SemanticError(Annot::new(kind, loc))
    }

    pub fn loc(&self) -> &Loc {
        &self.0.loc
    }

    pub fn show_diagnostic(&self, input: &str) {
        let mut message = String::new();
        self.loc()
            .annotate(&mut message, input)
            .expect("failed to generate error message.");
        eprintln!("{}", message);
    }
}

impl fmt::Display for SemanticError {
    fn fmt(&self, _f: &mut fmt::Formatter) -> fmt::Result {
        use self::SemanticErrorKind::*;
        match self.0.value {
            _ => Ok(()),
        }
    }
}

pub fn analyze(mut ast: Ast) -> Result<Ast> {
    Ok(ast)
}
