use failure::Fail;
use std::collections::HashMap;
use std::fmt;
use std::iter::Peekable;
use std::str::FromStr;

use super::lexer::{Annot, Keyword, LexError, Lexer, Loc, Token, TokenKind, TypeName};

#[cfg(test)]
#[cfg_attr(tarpaulin, skip)]
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
    NeedTokenBefore(Token, Vec<TokenKind>),
    NeedTypeNameBefore(Token),
    Redefinition(Token),
    Undeclared(Token),
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
            | UnclosedOpenParen(Token { ref loc, .. })
            | NeedTokenBefore(Token { ref loc, .. }, _)
            | NeedTypeNameBefore(Token { ref loc, .. })
            | Redefinition(Token { ref loc, .. })
            | Undeclared(Token { ref loc, .. }) => loc,
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
        eprintln!("{}", message);
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
            NeedTokenBefore(token, expected) => {
                let len = expected.len();
                match len {
                    1 => write!(
                        f,
                        "{}: expected '{}' before '{}'",
                        token.loc, expected[0], token.value
                    ),
                    2 => write!(
                        f,
                        "{}: expected '{}' or '{}' before '{}'",
                        token.loc, expected[0], expected[1], token.value
                    ),
                    _ => {
                        write!(f, "{}: expected one of ", token.loc)?;
                        for kind in &expected[..len - 1] {
                            write!(f, "'{}', ", kind)?;
                        }
                        write!(f, "or '{}' before '{}'", expected[len - 1], token.value)
                    }
                }
            }
            NeedTypeNameBefore(token) => write!(
                f,
                "{}: expected type name before '{}'",
                token.loc, token.value
            ),
            Redefinition(token) => write!(f, "{}: redefinition of '{}'", token.loc, token.value),
            Undeclared(token) => write!(f, "{}: '{}' undeclared", token.loc, token.value),
            Eof => write!(f, "unexpected end of file"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Int,
    Ptr(Box<Type>),
}

impl Type {
    fn ptr(ty: Type) -> Self {
        Type::Ptr(Box::new(ty))
    }
}

impl From<TypeName> for Type {
    fn from(type_name: TypeName) -> Self {
        match type_name {
            TypeName::Int => Type::Int,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstNode {
    Program {
        funcs: Vec<Ast>,
    },
    Func {
        name: String,
        args: Vec<(String, Type)>,
        lvars: HashMap<String, Type>,
        body: Box<Ast>,
    },
    Block(Vec<Ast>),
    StmtIf {
        cond: Box<Ast>,
        stmt: Box<Ast>,
        els: Option<Box<Ast>>,
    },
    StmtWhile {
        cond: Box<Ast>,
        stmt: Box<Ast>,
    },
    StmtFor {
        init: Option<Box<Ast>>,
        cond: Option<Box<Ast>>,
        incr: Option<Box<Ast>>,
        stmt: Box<Ast>,
    },
    StmtNull,
    Num(u64),
    Ident(String),
    BinOp {
        op: BinOp,
        l: Box<Ast>,
        r: Box<Ast>,
    },
    UniOp {
        op: UniOp,
        e: Box<Ast>,
    },
    Ret {
        e: Box<Ast>,
    },
    FuncCall {
        name: String,
        args: Vec<Ast>,
    },
}

pub type Ast = Annot<AstNode>;

impl Ast {
    fn program(funcs: Vec<Ast>, loc: Loc) -> Self {
        Self::new(AstNode::Program { funcs }, loc)
    }
    fn func(
        name: String,
        args: Vec<(String, Type)>,
        lvars: HashMap<String, Type>,
        body: Ast,
        loc: Loc,
    ) -> Self {
        Self::new(
            AstNode::Func {
                name,
                args,
                lvars,
                body: Box::new(body),
            },
            loc,
        )
    }
    fn block(stmts: Vec<Ast>, loc: Loc) -> Self {
        Self::new(AstNode::Block(stmts), loc)
    }
    fn stmt_if(cond: Ast, stmt: Ast, els: Option<Ast>, loc: Loc) -> Self {
        Self::new(
            AstNode::StmtIf {
                cond: Box::new(cond),
                stmt: Box::new(stmt),
                els: els.map(|els| Box::new(els)),
            },
            loc,
        )
    }
    fn stmt_while(cond: Ast, stmt: Ast, loc: Loc) -> Self {
        Self::new(
            AstNode::StmtWhile {
                cond: Box::new(cond),
                stmt: Box::new(stmt),
            },
            loc,
        )
    }
    fn stmt_for(
        init: Option<Ast>,
        cond: Option<Ast>,
        incr: Option<Ast>,
        stmt: Ast,
        loc: Loc,
    ) -> Self {
        Self::new(
            AstNode::StmtFor {
                init: init.map(|init| Box::new(init)),
                cond: cond.map(|cond| Box::new(cond)),
                incr: incr.map(|incr| Box::new(incr)),
                stmt: Box::new(stmt),
            },
            loc,
        )
    }
    fn stmt_null(loc: Loc) -> Self {
        Self::new(AstNode::StmtNull, loc)
    }
    fn num(n: u64, loc: Loc) -> Self {
        // call Annot::new
        Self::new(AstNode::Num(n), loc)
    }
    fn ident(name: String, loc: Loc) -> Self {
        Self::new(AstNode::Ident(name), loc)
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
    fn ret(e: Ast, loc: Loc) -> Self {
        Self::new(AstNode::Ret { e: Box::new(e) }, loc)
    }
    fn funcall(name: String, args: Vec<Ast>, loc: Loc) -> Self {
        Self::new(AstNode::FuncCall { name, args }, loc)
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
    Assign,
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
    fn assign(loc: Loc) -> Self {
        Self::new(BinOpKind::Assign, loc)
    }
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

#[derive(Debug, Clone, PartialEq, Eq)]
struct Context {
    args: Vec<(String, Type)>,
    lvars: HashMap<String, Type>,
}

fn consume<T>(tokens: &mut Peekable<T>, kind: TokenKind) -> Result<Loc>
where
    T: Iterator<Item = Token>,
{
    match tokens.next() {
        Some(token) => {
            if token.value == kind {
                Ok(token.loc)
            } else {
                Err(ParseError::NeedTokenBefore(token, vec![kind]))
            }
        }
        None => Err(ParseError::Eof),
    }
}

fn consume_ident<T>(tokens: &mut Peekable<T>) -> Result<(String, Loc)>
where
    T: Iterator<Item = Token>,
{
    match tokens.next() {
        Some(Token {
            value: TokenKind::Ident(name),
            loc,
        }) => Ok((name, loc)),
        Some(token) => Err(ParseError::NeedTokenBefore(
            token,
            vec![TokenKind::Ident("".to_string())],
        )),
        None => Err(ParseError::Eof),
    }
}

fn consume_type_name<T>(tokens: &mut Peekable<T>) -> Result<(TypeName, Loc)>
where
    T: Iterator<Item = Token>,
{
    match tokens.next() {
        Some(Token {
            value: TokenKind::TypeName(ty),
            loc,
        }) => Ok((ty, loc)),
        Some(token) => Err(ParseError::NeedTypeNameBefore(token)),
        None => Err(ParseError::Eof),
    }
}

/// Parse tokens with following rules
///
/// program     = func*
/// func        = "int" ident "(" ("int" declarator ("," "int" declarator)*)? ")" (";" | block)
/// stmt        = expr ";"
///             | block
///             | stmt_if
///             | stmt_while
///             | stmt_for
///             | stmt_return
///             | declaration ";"
/// block       = "{" stmt* "}"
/// stmt_if     = "if" "(" expr ")" stmt ("else" stmt)?
/// stmt_while  = "while" "(" expr ")" stmt
/// stmt_for    = "for" "(" (declaration | expr)? ";" expr? ";" expr? ")" stmt
/// stmt_return = "return" expr ";"
/// declaration = "int" declarator ("=" assign)?
/// declarator  = ("*")* ident
/// expr        = assign
/// assign      = equality ("=" assign)?
/// equality    = relational ("==" relational | "!=" relational)*
/// relational  = add ("<" add | "<=" add | ">" add | ">=" add)*
/// add         = mul ("+" mul | "-" mul)*
/// mul         = unary ("*" unary | "/" unary)*
/// unary       = ("+" | "-")? term
/// term        = num
///             | ident
///             | ident "(" (expr ("," expr)*)? ")"
///             | "(" expr ")"
fn parse(tokens: Vec<Token>) -> Result<Ast> {
    debug!("parse --");

    let mut tokens = tokens.into_iter().peekable();
    let ret = parse_program(&mut tokens)?;
    let ret = match tokens.next() {
        Some(token) => Err(ParseError::RedundantExpression(token)),
        None => Ok(ret),
    };

    debug!("parse: {:?}", ret);
    ret
}

/// Parse program
///
/// program     = func*
fn parse_program<T>(tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_program --");

    let mut loc = Loc::NONE;
    let mut funcs = vec![];
    while let Some(_) = tokens.peek() {
        let func = parse_func(tokens)?;
        if let Some(func) = func {
            loc = loc.merge(&func.loc);
            funcs.push(func);
        }
    }
    let ret = Ok(Ast::program(funcs, loc));

    debug!("parse_program: {:?}", ret);
    ret
}

/// Parse func
///
/// func        = "int" ident "(" ("int" declarator ("," "int" declarator)*)? ")" (";" | block)
fn parse_func<T>(tokens: &mut Peekable<T>) -> Result<Option<Ast>>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_func --");

    let mut ctx = Context {
        args: Vec::new(),
        lvars: HashMap::new(),
    };

    let (_, loc) = consume_type_name(tokens)?;
    let (name, _) = consume_ident(tokens)?;

    consume(tokens, TokenKind::LParen)?;
    loop {
        if let Some(TokenKind::RParen) = tokens.peek().map(|token| &token.value) {
            break;
        }
        if ctx.args.len() > 0 {
            consume(tokens, TokenKind::Comma)?;
        }
        let (type_name, _) = consume_type_name(tokens)?;
        let (arg, ty, _) = parse_declarator(&mut ctx, tokens, type_name.into())?;
        // TODO: check duplicate arg name
        ctx.args.push((arg, ty));
    }
    consume(tokens, TokenKind::RParen)?;

    let ret = match tokens.peek().map(|token| &token.value) {
        Some(TokenKind::Semicolon) => {
            tokens.next().unwrap();
            // TODO: do not skip function declaration
            Ok(None)
        }
        _ => {
            let block = parse_block(&mut ctx, tokens)?;
            let loc = loc.merge(&block.loc);
            Ok(Some(Ast::func(name, ctx.args, ctx.lvars, block, loc)))
        }
    };

    debug!("parse_func: {:?}", ret);
    ret
}

/// Parse stmt
///
/// stmt        = expr ";"
///             | block
///             | stmt_if
///             | stmt_while
///             | stmt_for
///             | stmt_return
fn parse_stmt<T>(ctx: &mut Context, tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_stmt --");

    let stmt = match tokens.peek().map(|token| &token.value) {
        Some(TokenKind::LBrace) => parse_block(ctx, tokens)?,
        Some(TokenKind::Keyword(Keyword::If)) => parse_stmt_if(ctx, tokens)?,
        Some(TokenKind::Keyword(Keyword::While)) => parse_stmt_while(ctx, tokens)?,
        Some(TokenKind::Keyword(Keyword::For)) => parse_stmt_for(ctx, tokens)?,
        Some(TokenKind::Keyword(Keyword::Return)) => parse_stmt_return(ctx, tokens)?,
        Some(TokenKind::TypeName(_)) => {
            let declaration = parse_declaration(ctx, tokens)?;
            let semi_loc = consume(tokens, TokenKind::Semicolon)?;
            Ast::new(declaration.value, declaration.loc.merge(&semi_loc))
        }
        _ => {
            let e = parse_expr(ctx, tokens)?;
            let semi_loc = consume(tokens, TokenKind::Semicolon)?;
            Ast::new(e.value, e.loc.merge(&semi_loc))
        }
    };

    let ret = Ok(stmt);
    debug!("parse_stmt: {:?}", ret);
    ret
}

/// Parse block
///
/// block       = "{" stmt* "}"
fn parse_block<T>(ctx: &mut Context, tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_block --");

    let loc = consume(tokens, TokenKind::LBrace)?;
    let mut stmts = vec![];
    while let Some(token) = tokens.peek() {
        if token.value == TokenKind::RBrace {
            break;
        }
        let stmt = parse_stmt(ctx, tokens)?;
        stmts.push(stmt);
    }
    let loc = loc.merge(&consume(tokens, TokenKind::RBrace)?);
    let ret = Ok(Ast::block(stmts, loc));

    debug!("parse_block: {:?}", ret);
    ret
}

/// Parse stmt_if
///
/// stmt_if     = "if" "(" expr ")" stmt ("else" stmt)?
fn parse_stmt_if<T>(ctx: &mut Context, tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_stmt_if --");

    let if_loc = consume(tokens, TokenKind::Keyword(Keyword::If))?;
    consume(tokens, TokenKind::LParen)?;
    let cond = parse_expr(ctx, tokens)?;
    consume(tokens, TokenKind::RParen)?;
    let stmt = parse_stmt(ctx, tokens)?;

    let ret = match tokens.peek().map(|token| &token.value) {
        Some(TokenKind::Keyword(Keyword::Else)) => {
            tokens.next();
            let els = parse_stmt(ctx, tokens)?;
            let loc = if_loc.merge(&els.loc);
            Ok(Ast::stmt_if(cond, stmt, Some(els), loc))
        }
        _ => {
            let loc = if_loc.merge(&stmt.loc);
            Ok(Ast::stmt_if(cond, stmt, None, loc))
        }
    };

    debug!("parse_stmt_if: {:?}", ret);
    ret
}

/// Parse stmt_while
///
/// stmt_while  = "while" "(" expr ")" stmt
fn parse_stmt_while<T>(ctx: &mut Context, tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_stmt_while --");

    let while_loc = consume(tokens, TokenKind::Keyword(Keyword::While))?;
    consume(tokens, TokenKind::LParen)?;
    let cond = parse_expr(ctx, tokens)?;
    consume(tokens, TokenKind::RParen)?;
    let stmt = parse_stmt(ctx, tokens)?;

    let loc = while_loc.merge(&stmt.loc);
    let ret = Ok(Ast::stmt_while(cond, stmt, loc));

    debug!("parse_stmt_while: {:?}", ret);
    ret
}

/// Parse stmt_for
///
/// stmt_for    = "for" "(" (declaration | expr)? ";" expr? ";" expr? ")" stmt
fn parse_stmt_for<T>(ctx: &mut Context, tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_stmt_for --");

    let for_loc = consume(tokens, TokenKind::Keyword(Keyword::For))?;
    consume(tokens, TokenKind::LParen)?;
    let init = match tokens.peek().map(|token| &token.value) {
        Some(TokenKind::Semicolon) => None,
        Some(TokenKind::TypeName(_)) => Some(parse_declaration(ctx, tokens)?),
        _ => Some(parse_expr(ctx, tokens)?),
    };
    consume(tokens, TokenKind::Semicolon)?;
    let cond = match tokens.peek().map(|token| &token.value) {
        Some(TokenKind::Semicolon) => None,
        _ => Some(parse_expr(ctx, tokens)?),
    };
    consume(tokens, TokenKind::Semicolon)?;
    let incr = match tokens.peek().map(|token| &token.value) {
        Some(TokenKind::Semicolon) => None,
        _ => Some(parse_expr(ctx, tokens)?),
    };
    consume(tokens, TokenKind::RParen)?;
    let stmt = parse_stmt(ctx, tokens)?;

    let loc = for_loc.merge(&stmt.loc);
    let ret = Ok(Ast::stmt_for(init, cond, incr, stmt, loc));

    debug!("parse_stmt_for: {:?}", ret);
    ret
}

/// Parse stmt_return
///
/// stmt_return = "return" expr ";"
fn parse_stmt_return<T>(ctx: &mut Context, tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_stmt_return --");

    let ret_loc = consume(tokens, TokenKind::Keyword(Keyword::Return))?;
    let e = parse_expr(ctx, tokens)?;
    let semi_loc = consume(tokens, TokenKind::Semicolon)?;
    let ret = Ok(Ast::ret(e, ret_loc.merge(&semi_loc)));

    debug!("parse_stmt_return: {:?}", ret);
    ret
}

/// Parse declaration
///
/// declaration = "int" declarator ("=" assign)?
fn parse_declaration<T>(ctx: &mut Context, tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_declaration --");

    let (type_name, loc) = consume_type_name(tokens)?;
    let (name, ty, ident_loc) = parse_declarator(ctx, tokens, type_name.into())?;
    if ctx.lvars.insert(name.clone(), ty).is_some() {
        // already declared
        return Err(ParseError::Redefinition(Token::ident(&name, ident_loc)));
    }

    let ret = match tokens.peek().map(|token| &token.value) {
        Some(TokenKind::Assign) => {
            let op = BinOp::assign(consume(tokens, TokenKind::Assign)?);
            let e = Ast::ident(name, ident_loc);
            let r = parse_assign(ctx, tokens)?;
            let loc = loc.merge(&r.loc);
            Ok(Ast::binop(op, e, r, loc))
        }
        _ => Ok(Ast::stmt_null(loc.merge(&ident_loc))),
    };

    debug!("parse_declaration: {:?}", ret);
    ret
}

/// Parse declarator
///
/// declarator  = ("*")* ident
fn parse_declarator<T>(
    _ctx: &mut Context,
    tokens: &mut Peekable<T>,
    mut ty: Type,
) -> Result<(String, Type, Loc)>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_declarator --");

    let mut loc = Loc::NONE;
    while let Some(TokenKind::Asterisk) = tokens.peek().map(|token| &token.value) {
        loc = loc.merge(&consume(tokens, TokenKind::Asterisk)?);
        ty = Type::ptr(ty);
    }

    let (name, ident_loc) = consume_ident(tokens)?;
    let ret = Ok((name, ty, loc.merge(&ident_loc)));

    debug!("parse_declarator: {:?}", ret);
    ret
}

/// Parse expr
///
/// expr        = assign
fn parse_expr<T>(ctx: &mut Context, tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_expr --");

    let ret = parse_assign(ctx, tokens);

    debug!("parse_expr: {:?}", ret);
    ret
}

/// Parse assign
///
/// assign      = equality ("=" assign)?
fn parse_assign<T>(ctx: &mut Context, tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_assign --");

    let e = parse_equality(ctx, tokens)?;

    let ret = match tokens.peek() {
        Some(Token {
            value: TokenKind::Assign,
            ..
        }) => {
            let op = BinOp::assign(tokens.next().unwrap().loc);
            let r = parse_assign(ctx, tokens)?;
            let loc = e.loc.merge(&r.loc);
            Ok(Ast::binop(op, e, r, loc))
        }
        _ => Ok(e),
    };

    debug!("parse_assign: {:?}", ret);
    ret
}

/// Parse equality
///
/// equality    = relational ("==" relational | "!=" relational)*
fn parse_equality<T>(ctx: &mut Context, tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_equality --");

    let mut e = parse_relational(ctx, tokens)?;

    while let Some(token) = tokens.peek() {
        let op = match token.value {
            TokenKind::Eq => BinOp::eq(tokens.next().unwrap().loc),
            TokenKind::Ne => BinOp::ne(tokens.next().unwrap().loc),
            _ => break,
        };
        let r = parse_relational(ctx, tokens)?;
        let loc = e.loc.merge(&r.loc);
        e = Ast::binop(op, e, r, loc);
    }

    let ret = Ok(e);
    debug!("parse_equality: {:?}", ret);
    ret
}

/// Parse relational
///
/// relational  = add ("<" add | "<=" add | ">" add | ">=" add)*
fn parse_relational<T>(ctx: &mut Context, tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_relational --");

    let mut e = parse_add(ctx, tokens)?;

    while let Some(token) = tokens.peek() {
        let op = match token.value {
            TokenKind::Lt => BinOp::lt(tokens.next().unwrap().loc),
            TokenKind::Le => BinOp::le(tokens.next().unwrap().loc),
            TokenKind::Gt => BinOp::gt(tokens.next().unwrap().loc),
            TokenKind::Ge => BinOp::ge(tokens.next().unwrap().loc),
            _ => break,
        };
        let r = parse_add(ctx, tokens)?;
        let loc = e.loc.merge(&r.loc);
        e = Ast::binop(op, e, r, loc);
    }

    let ret = Ok(e);
    debug!("parse_relational: {:?}", ret);
    ret
}

/// Parse add
///
/// add         = mul ("+" mul | "-" mul)*
fn parse_add<T>(ctx: &mut Context, tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_add --");

    let mut e = parse_mul(ctx, tokens)?;

    while let Some(token) = tokens.peek() {
        let op = match token.value {
            TokenKind::Plus => BinOp::add(tokens.next().unwrap().loc),
            TokenKind::Minus => BinOp::sub(tokens.next().unwrap().loc),
            _ => break,
        };
        let r = parse_mul(ctx, tokens)?;
        let loc = e.loc.merge(&r.loc);
        e = Ast::binop(op, e, r, loc);
    }

    let ret = Ok(e);
    debug!("parse_add: {:?}", ret);
    ret
}

/// Parse mul
///
/// mul         = unary ("*" unary | "/" unary)*
fn parse_mul<T>(ctx: &mut Context, tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_mul --");

    let mut e = parse_unary(ctx, tokens)?;

    while let Some(token) = tokens.peek() {
        let op = match token.value {
            TokenKind::Asterisk => BinOp::mul(tokens.next().unwrap().loc),
            TokenKind::Slash => BinOp::div(tokens.next().unwrap().loc),
            _ => break,
        };
        let r = parse_unary(ctx, tokens)?;
        let loc = e.loc.merge(&r.loc);
        e = Ast::binop(op, e, r, loc);
    }

    let ret = Ok(e);
    debug!("parse_mul: {:?}", ret);
    ret
}

/// Parse unary
///
/// unary       = ("+" | "-")? term
fn parse_unary<T>(ctx: &mut Context, tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_unary --");

    let ret = match tokens.peek().map(|token| &token.value) {
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
            let e = parse_term(ctx, tokens)?;
            let loc = op.loc.merge(&e.loc);
            Ok(Ast::uniop(op, e, loc))
        }
        _ => parse_term(ctx, tokens),
    };

    debug!("parse_unary: {:?}", ret);
    ret
}

/// Parse term
///
/// term        = num
///             | ident
///             | ident "(" (expr ("," expr)*)? ")"
///             | "(" expr ")"
fn parse_term<T>(ctx: &mut Context, tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_term --");

    let token = tokens.next().ok_or(ParseError::Eof)?;
    let ret = match token.value {
        TokenKind::Number(n) => Ok(Ast::num(n, token.loc)),
        TokenKind::Ident(name) => match tokens.peek().map(|token| &token.value) {
            Some(TokenKind::LParen) => {
                consume(tokens, TokenKind::LParen)?;
                let mut args = Vec::new();
                loop {
                    if let Some(TokenKind::RParen) = tokens.peek().map(|token| &token.value) {
                        break;
                    }
                    if args.len() > 0 {
                        consume(tokens, TokenKind::Comma)?;
                    }
                    args.push(parse_expr(ctx, tokens)?);
                }
                let loc = consume(tokens, TokenKind::RParen)?;
                Ok(Ast::funcall(name, args, token.loc.merge(&loc)))
            }
            _ => {
                // TODO: support env
                if ctx.args.iter().find(|&(item, _)| item == &name).is_some()
                    || ctx.lvars.get(&name).is_some()
                {
                    Ok(Ast::ident(name, token.loc))
                } else {
                    Err(ParseError::Undeclared(Token::ident(&name, token.loc)))
                }
            }
        },
        TokenKind::LParen => {
            let e = parse_expr(ctx, tokens)?;
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
