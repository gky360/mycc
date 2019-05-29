use std::fmt;
use std::iter::Peekable;
use std::str::FromStr;

use super::lexer::{Annot, LexError, Lexer, Loc, Token, TokenKind};

pub type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ParseError {
    #[allow(dead_code)]
    UnexpectedToken(Token),
    NotExpression(Token),
    NotOperator(Token),
    UnclosedOpenParen(Token),
    RedundantExpression(Token),
    Eof,
}

impl std::error::Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ParseError::*;
        match self {
            UnexpectedToken(token) => write!(f, "{}: {} is not expected", token.loc, token.value),
            NotExpression(token) => write!(
                f,
                "{}: '{}' is not a start of expression",
                token.loc, token.value
            ),
            NotOperator(token) => write!(f, "{}: '{}' is not an operator", token.loc, token.value),
            UnclosedOpenParen(token) => write!(f, "{}: '{}' is not closed", token.loc, token.value),
            RedundantExpression(token) => write!(
                f,
                "{}: expression after '{}' is redundant",
                token.loc, token.value
            ),
            Eof => write!(f, "End of file"),
        }
    }
}