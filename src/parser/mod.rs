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

pub fn print_annot(input: &str, loc: &Loc) {
    eprintln!("{}", input);
    eprintln!("{}{}", " ".repeat(loc.0), "^".repeat(loc.1 - loc.0));
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Error {
    Lexer(LexError),
    Parser(ParseError),
}

impl Error {
    pub fn show_diagnostic(&self, input: &str) {
        use self::Error::*;
        use self::ParseError as P;
        let temp_loc;
        let (err, loc): (&std::error::Error, &Loc) = match self {
            Lexer(err) => (err, &err.loc),
            Parser(err) => (
                err,
                match err {
                    P::UnexpectedToken(Token { loc, .. })
                    | P::NotExpression(Token { loc, .. })
                    | P::NotOperator(Token { loc, .. })
                    | P::UnclosedOpenParen(Token { loc, .. }) => loc,
                    P::RedundantExpression(Token { loc, .. }) => {
                        temp_loc = Loc(loc.0, input.len());
                        &temp_loc
                    }
                    P::Eof => {
                        temp_loc = Loc(input.len(), input.len() + 1);
                        &temp_loc
                    }
                },
            ),
        };
        eprintln!("{}", err);
        print_annot(input, loc);
    }
}

impl From<LexError> for Error {
    fn from(e: LexError) -> Self {
        Error::Lexer(e)
    }
}

impl From<ParseError> for Error {
    fn from(e: ParseError) -> Self {
        Error::Parser(e)
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        use self::Error::*;
        match self {
            Lexer(err) => Some(err),
            Parser(err) => Some(err),
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "parser error")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AstNode {
    Num(u64),
    UniOp { op: UniOp, e: Box<Ast> },
    BinOp { op: BinOp, l: Box<Ast>, r: Box<Ast> },
}

pub type Ast = Annot<AstNode>;

impl Ast {
    fn num(n: u64, loc: Loc) -> Self {
        // call Annot::new
        Self::new(AstNode::Num(n), loc)
    }
    fn uniop(op: UniOp, e: Ast, loc: Loc) -> Self {
        Self::new(AstNode::UniOp { op, e: Box::new(e) }, loc)
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
}

impl FromStr for Ast {
    type Err = Error;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let tokens = Lexer::new(s).lex()?;
        let ast = parse(tokens)?;
        Ok(ast)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UniOpKind {
    Plus,
    Minus,
}

pub type UniOp = Annot<UniOpKind>;

impl UniOp {
    fn plus(loc: Loc) -> Self {
        Self::new(UniOpKind::Plus, loc)
    }
    fn minus(loc: Loc) -> Self {
        Self::new(UniOpKind::Minus, loc)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinOpKind {
    Add,
    Sub,
    Mul,
    Div,
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
}

fn parse(tokens: Vec<Token>) -> Result<Ast> {
    let mut tokens = tokens.into_iter().peekable();
    let ret = parse_expr(&mut tokens)?;
    let ret = match tokens.next() {
        Some(token) => Err(ParseError::RedundantExpression(token)),
        None => Ok(ret),
    };
    // eprintln!("parse result: {:?}", ret);
    ret
}

/// Parse EXPR
///
/// EXPR = EXPR3
fn parse_expr<T>(tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    // eprintln!("EXPR --");
    let ret = parse_expr3(tokens);
    // eprintln!("EXPR: {:?}", ret);
    ret
}

/// Parse binpop
///
/// expr = subexpr expr_Loop
/// expr_Loop = op subexpr expr_Loop | eps
fn parse_left_binop<T>(
    tokens: &mut Peekable<T>,
    subexpr_parser: fn(&mut Peekable<T>) -> Result<Ast>,
    op_parser: fn(&mut Peekable<T>) -> Result<BinOp>,
) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    // subexpr
    let mut e = subexpr_parser(tokens)?;

    // expr_loop
    loop {
        match tokens.peek() {
            Some(_) => {
                // op
                let op = match op_parser(tokens) {
                    Ok(op) => op,
                    // no binops any more
                    Err(_) => break,
                };
                // subexpr
                let r = subexpr_parser(tokens)?;
                let loc = e.loc.merge(&r.loc);
                e = Ast::binop(op, e, r, loc);
            }
            // eps
            _ => break,
        }
    }

    Ok(e)
}

fn parse_expr3_op<T>(tokens: &mut Peekable<T>) -> Result<BinOp>
where
    T: Iterator<Item = Token>,
{
    // eprintln!("expr3_op --");
    let token = match tokens.peek() {
        None => return Err(ParseError::Eof),
        Some(token) => token,
    };
    let ret = match token.value {
        TokenKind::Plus => Ok(BinOp::add(tokens.next().unwrap().loc)),
        TokenKind::Minus => Ok(BinOp::sub(tokens.next().unwrap().loc)),
        _ => Err(ParseError::NotOperator(token.clone())),
    };
    // eprintln!("expr3_op: {:?}", ret);
    ret
}

/// Parse EXPR3
///
/// EXPR3 = EXPR2 EXPR3_Loop
/// EXPR3_Loop = ("+" | "-") EXPR2 EXPR3_Loop | eps
fn parse_expr3<T>(tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    // eprintln!("EXPR3 --");
    let ret = parse_left_binop(tokens, parse_expr2, parse_expr3_op);
    // eprintln!("EXPR3: {:?}", ret);
    ret
}

fn parse_expr2_op<T>(tokens: &mut Peekable<T>) -> Result<BinOp>
where
    T: Iterator<Item = Token>,
{
    // eprintln!("expr2_op --");
    let token = match tokens.peek() {
        None => return Err(ParseError::Eof),
        Some(token) => token,
    };
    let ret = match token.value {
        TokenKind::Asterisk => Ok(BinOp::mul(tokens.next().unwrap().loc)),
        TokenKind::Slash => Ok(BinOp::div(tokens.next().unwrap().loc)),
        _ => Err(ParseError::NotOperator(token.clone())),
    };
    // eprintln!("expr2_op: {:?}", ret);
    ret
}

/// Parse EXPR2
///
/// EXPR2 = EXPR1 EXPR2_Loop
/// EXPR2_Loop = ("*" | "/") EXPR1 EXPR2_Loop | eps
fn parse_expr2<T>(tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    // eprintln!("EXPR2 --");
    let ret = parse_left_binop(tokens, parse_expr1, parse_expr2_op);
    // eprintln!("EXPR2: {:?}", ret);
    ret
}

/// Parse EXPR1
///
/// EXPR1 = ("+" | "-"), ATOM | ATOM
fn parse_expr1<T>(tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    // eprintln!("EXPR1 --");
    let ret = match tokens.peek().map(|token| token.value) {
        Some(TokenKind::Plus) | Some(TokenKind::Minus) => {
            // ("+" | "-")
            let op = match tokens.next() {
                Some(Token {
                    value: TokenKind::Plus,
                    loc,
                }) => UniOp::plus(loc),
                Some(Token {
                    value: TokenKind::Minus,
                    loc,
                }) => UniOp::minus(loc),
                _ => unreachable!(),
            };
            // , ATOM
            let e = parse_atom(tokens)?;
            let loc = op.loc.merge(&e.loc);
            Ok(Ast::uniop(op, e, loc))
        }
        // | ATOM
        _ => parse_atom(tokens),
    };
    // eprintln!("EXPR1: {:?}", ret);
    ret
}

/// Parse ATOM
///
/// ATOM = UNUMBER | "(", EXPR3, ")"
fn parse_atom<T>(tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    // eprintln!("ATOM --");
    let ret = tokens
        .next()
        .ok_or(ParseError::Eof)
        .and_then(|token| match token.value {
            // UNUMBER
            TokenKind::Number(n) => Ok(Ast::num(n, token.loc)),
            // "(", EXPR3, ")"
            TokenKind::LParen => {
                let e = parse_expr3(tokens)?;
                match tokens.next() {
                    Some(Token {
                        value: TokenKind::RParen,
                        ..
                    }) => Ok(e),
                    Some(token) => Err(ParseError::RedundantExpression(token)),
                    _ => Err(ParseError::UnclosedOpenParen(token)),
                }
            }
            _ => Err(ParseError::NotExpression(token)),
        });
    // eprintln!("ATOM: {:?}", ret);
    ret
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser() {
        // 1 + 2 * 3 - -10
        let ast = parse(vec![
            Token::number(1, Loc(0, 1)),
            Token::plus(Loc(2, 3)),
            Token::number(2, Loc(4, 5)),
            Token::asterisk(Loc(6, 7)),
            Token::number(3, Loc(8, 9)),
            Token::minus(Loc(10, 11)),
            Token::minus(Loc(12, 13)),
            Token::number(10, Loc(13, 15)),
        ]);
        assert_eq!(
            ast,
            Ok(Ast::binop(
                BinOp::sub(Loc(10, 11)),
                Ast::binop(
                    BinOp::add(Loc(2, 3)),
                    Ast::num(1, Loc(0, 1)),
                    Ast::binop(
                        BinOp::mul(Loc(6, 7)),
                        Ast::num(2, Loc(4, 5)),
                        Ast::num(3, Loc(8, 9)),
                        Loc(4, 9)
                    ),
                    Loc(0, 9)
                ),
                Ast::uniop(
                    UniOp::minus(Loc(12, 13)),
                    Ast::num(10, Loc(13, 15)),
                    Loc(12, 15)
                ),
                Loc(0, 15)
            ))
        )
    }
}
