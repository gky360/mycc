use failure::Fail;
use std::io::Write;
use std::iter::Peekable;
use std::str::FromStr;
use std::{fmt, io};

use super::lexer::{Annot, LexError, Lexer, Loc, Token, TokenKind};

#[cfg(test)]
mod tests;

pub type Result<T> = std::result::Result<T, ParseError>;

#[derive(Fail, Debug, Clone, PartialEq, Eq, Hash)]
pub enum ParseError {
    Lex(#[fail(cause)] LexError),
    UnexpectedToken(Token),
    NotExpression(Token),
    NotOperator(Token),
    UnclosedOpenParen(Token),
    RedundantExpression(Token),
    Eof,
}

impl ParseError {
    pub fn show_diagnostic(&self, input: &str) {
        use self::ParseError::*;
        let temp_loc;
        let loc = match self {
            Lex(err) => err.loc(),
            UnexpectedToken(Token { ref loc, .. })
            | NotExpression(Token { ref loc, .. })
            | NotOperator(Token { ref loc, .. })
            | UnclosedOpenParen(Token { ref loc, .. }) => loc,
            RedundantExpression(Token { loc, .. }) => {
                temp_loc = Loc(loc.0, input.len());
                &temp_loc
            }
            Eof => {
                temp_loc = Loc(input.len(), input.len() + 1);
                &temp_loc
            }
        };
        let mut message = String::new();
        loc.annotate(&mut message, input)
            .expect("failed to generate error message.");
        write!(io::stderr(), "{}", message).expect("failed to output error message.");
    }
}

impl From<LexError> for ParseError {
    fn from(err: LexError) -> ParseError {
        ParseError::Lex(err)
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ParseError::*;
        match self {
            Lex(_) => write!(f, "invalid token"),
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


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstNode {
    Num(u64),
    BinOp { op: BinOp, l: Box<Ast>, r: Box<Ast> },
    UniOp { op: UniOp, e: Box<Ast> },
}

pub type Ast = Annot<AstNode>;

impl Ast {
    fn num(n: u64, loc: Loc) -> Self {
        // call Annot::new
        Self::new(AstNode::Num(n), loc)
    }
    fn binop(op: BinOp, l: Ast, r: Ast, loc: Loc) -> Self {
        Self::new(
            AstNode::BinOp {
                op,
                l: Box::new(l),
                r: Box::new(r),
            },
            loc,
        )
    }
    fn uniop(op: UniOp, e: Ast, loc: Loc) -> Self {
        Self::new(AstNode::UniOp { op, e: Box::new(e) }, loc)
    }
}

impl FromStr for Ast {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Ast> {
        let tokens = Lexer::new(s).lex()?;
        let ast = parse(tokens)?;
        Ok(ast)
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

pub type BinOp = Annot<BinOpKind>;

impl BinOp {
    fn add(loc: Loc) -> Self {
        Self::new(BinOpKind::Add, loc)
    }
    fn sub(loc: Loc) -> Self {
        Self::new(BinOpKind::Sub, loc)
    }
    fn mul(loc: Loc) -> Self {
        Self::new(BinOpKind::Mul, loc)
    }
    fn div(loc: Loc) -> Self {
        Self::new(BinOpKind::Div, loc)
    }
    fn eq(loc: Loc) -> Self {
        Self::new(BinOpKind::Eq, loc)
    }
    fn ne(loc: Loc) -> Self {
        Self::new(BinOpKind::Ne, loc)
    }
    fn lt(loc: Loc) -> Self {
        Self::new(BinOpKind::Lt, loc)
    }
    fn le(loc: Loc) -> Self {
        Self::new(BinOpKind::Le, loc)
    }
    fn gt(loc: Loc) -> Self {
        Self::new(BinOpKind::Gt, loc)
    }
    fn ge(loc: Loc) -> Self {
        Self::new(BinOpKind::Ge, loc)
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UniOpKind {
    Positive,
    Negative,
}

pub type UniOp = Annot<UniOpKind>;

impl UniOp {
    fn positive(loc: Loc) -> Self {
        Self::new(UniOpKind::Positive, loc)
    }
    fn negative(loc: Loc) -> Self {
        Self::new(UniOpKind::Negative, loc)
    }
}


/// Parse tokens with following rules
///
/// expr       = equality
/// equality   = relational ("==" relational | "!=" relational)*
/// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
/// add        = mul ("+" mul | "-" mul)*
/// mul        = unary ("*" unary | "/" unary)*
/// unary      = ("+" | "-")? term
/// term       = num | "(" expr ")"
fn parse(tokens: Vec<Token>) -> Result<Ast> {
    debug!("parse --");

    let mut tokens = tokens.into_iter().peekable();
    let ret = parse_expr(&mut tokens)?;
    let ret = match tokens.next() {
        Some(token) => Err(ParseError::RedundantExpression(token)),
        None => Ok(ret),
    };

    debug!("parse: {:?}", ret);
    ret
}

/// Parse expr
///
/// expr       = equality
fn parse_expr<T>(tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_expr --");

    let ret = parse_equality(tokens);

    debug!("parse_expr: {:?}", ret);
    ret
}

/// Parse equality
///
/// equality   = relational ("==" relational | "!=" relational)*
fn parse_equality<T>(tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_equality --");

    let mut e = parse_relational(tokens)?;

    while let Some(token) = tokens.peek() {
        let op = match token.value {
            TokenKind::Eq => BinOp::eq(tokens.next().unwrap().loc),
            TokenKind::Ne => BinOp::ne(tokens.next().unwrap().loc),
            _ => break,
        };
        let r = parse_relational(tokens)?;
        let loc = e.loc.merge(&r.loc);
        e = Ast::binop(op, e, r, loc);
    }

    let ret = Ok(e);
    debug!("parse_equality: {:?}", ret);
    ret
}

/// Parse relational
///
/// relational = add ("<" add | "<=" add | ">" add | ">=" add)*
fn parse_relational<T>(tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_relational --");

    let mut e = parse_add(tokens)?;

    while let Some(token) = tokens.peek() {
        let op = match token.value {
            TokenKind::Lt => BinOp::lt(tokens.next().unwrap().loc),
            TokenKind::Le => BinOp::le(tokens.next().unwrap().loc),
            TokenKind::Gt => BinOp::gt(tokens.next().unwrap().loc),
            TokenKind::Ge => BinOp::ge(tokens.next().unwrap().loc),
            _ => break,
        };
        let r = parse_add(tokens)?;
        let loc = e.loc.merge(&r.loc);
        e = Ast::binop(op, e, r, loc);
    }

    let ret = Ok(e);
    debug!("parse_relational: {:?}", ret);
    ret
}

/// Parse add
///
/// add        = mul ("+" mul | "-" mul)*
fn parse_add<T>(tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_add --");

    let mut e = parse_mul(tokens)?;

    while let Some(token) = tokens.peek() {
        let op = match token.value {
            TokenKind::Plus => BinOp::add(tokens.next().unwrap().loc),
            TokenKind::Minus => BinOp::sub(tokens.next().unwrap().loc),
            _ => break,
        };
        let r = parse_mul(tokens)?;
        let loc = e.loc.merge(&r.loc);
        e = Ast::binop(op, e, r, loc);
    }

    let ret = Ok(e);
    debug!("parse_add: {:?}", ret);
    ret
}

/// Parse mul
///
/// mul        = unary ("*" unary | "/" unary)*
fn parse_mul<T>(tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_mul --");

    let mut e = parse_unary(tokens)?;

    while let Some(token) = tokens.peek() {
        let op = match token.value {
            TokenKind::Asterisk => BinOp::mul(tokens.next().unwrap().loc),
            TokenKind::Slash => BinOp::div(tokens.next().unwrap().loc),
            _ => break,
        };
        let r = parse_unary(tokens)?;
        let loc = e.loc.merge(&r.loc);
        e = Ast::binop(op, e, r, loc);
    }

    let ret = Ok(e);
    debug!("parse_mul: {:?}", ret);
    ret
}

/// Parse unary
///
/// unary      = ("+" | "-")? term
fn parse_unary<T>(tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_unary --");

    let ret = match tokens.peek().map(|token| token.value) {
        Some(TokenKind::Plus) | Some(TokenKind::Minus) => {
            let op = match tokens.next().unwrap() {
                Token {
                    value: TokenKind::Plus,
                    loc,
                } => UniOp::positive(loc),
                Token {
                    value: TokenKind::Minus,
                    loc,
                } => UniOp::negative(loc),
                _ => unreachable!(),
            };
            let e = parse_term(tokens)?;
            let loc = op.loc.merge(&e.loc);
            Ok(Ast::uniop(op, e, loc))
        }
        _ => parse_term(tokens),
    };

    debug!("parse_unary: {:?}", ret);
    ret
}

/// Parse term
///
/// term       = num | "(" expr ")"
fn parse_term<T>(tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_term --");

    let token = tokens.next().ok_or(ParseError::Eof)?;
    let ret = match token.value {
        TokenKind::Number(n) => Ok(Ast::num(n, token.loc)),
        TokenKind::LParen => {
            let e = parse_expr(tokens)?;
            match tokens.next() {
                Some(Token {
                    value: TokenKind::RParen,
                    ..
                }) => Ok(e),
                Some(token) => Err(ParseError::RedundantExpression(token)),
                None => Err(ParseError::UnclosedOpenParen(token)),
            }
        }
        _ => Err(ParseError::NotExpression(token)),
    };

    debug!("parse_term: {:?}", ret);
    ret
}