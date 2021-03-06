use failure::Fail;
use std::collections::HashMap;
use std::fmt;
use std::iter::Peekable;
use std::str::FromStr;

use super::lexer::{Keyword, LexError, Lexer, Loc, Token, TokenKind, TypeName};

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
    ConflictingTypes(Token),
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
            | Undeclared(Token { ref loc, .. })
            | ConflictingTypes(Token { ref loc, .. }) => loc,
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
            ConflictingTypes(token) => {
                write!(f, "{}: conflicting types for '{}'", token.loc, token.value)
            }
            Eof => write!(f, "unexpected end of file"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Annot<T> {
    pub value: T,
    pub loc: Loc,
    pub ty: Option<Type>,
}

impl<T> Annot<T> {
    pub fn new(value: T, loc: Loc) -> Self {
        Self {
            value,
            loc,
            ty: None,
        }
    }

    pub fn get_type(&self) -> &Type {
        self.ty.as_ref().expect("could not get type")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Int,
    Ptr(Box<Type>),
    Array(Box<Type>, usize),
}

impl Type {
    pub fn ptr(ty: Type) -> Self {
        Type::Ptr(Box::new(ty))
    }
    pub fn array(ty: Type, len: usize) -> Self {
        Type::Array(Box::new(ty), len)
    }

    pub fn size(&self) -> usize {
        match self {
            Type::Int => 4,
            Type::Ptr(_) => 8,
            Type::Array(ty, len) => ty.size() * len,
        }
    }
    pub fn qwords(&self) -> usize {
        (self.size() + 7) / 8
    }
}

impl From<TypeName> for Type {
    fn from(type_name: TypeName) -> Self {
        match type_name {
            TypeName::Int => Type::Int,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Ptr(ty) => write!(f, "*{}", ty),
            Type::Array(ty, len) => write!(f, "{}[{}]", ty, len),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var {
    pub name: String,
    pub ty: Type,
    pub is_local: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AstNode {
    Program {
        funcs: Vec<Ast>,
        gvars: HashMap<String, Var>,
    },
    Func {
        name: String,
        args: Vec<Var>,
        /// local variables including function arguments
        lvars: HashMap<String, Var>,
        ret_ty: Type,
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
    Num(usize),
    VarRef(Var),
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
        ret_ty: Type,
    },
}

pub type Ast = Annot<AstNode>;

impl Ast {
    pub fn program(funcs: Vec<Ast>, gvars: HashMap<String, Var>, loc: Loc) -> Self {
        Self::new(AstNode::Program { funcs, gvars }, loc)
    }
    pub fn func(
        name: String,
        args: Vec<Var>,
        lvars: HashMap<String, Var>,
        ret_ty: Type,
        body: Ast,
        loc: Loc,
    ) -> Self {
        Self::new(
            AstNode::Func {
                name,
                args,
                lvars,
                ret_ty,
                body: Box::new(body),
            },
            loc,
        )
    }
    pub fn block(stmts: Vec<Ast>, loc: Loc) -> Self {
        Self::new(AstNode::Block(stmts), loc)
    }
    pub fn stmt_if(cond: Ast, stmt: Ast, els: Option<Ast>, loc: Loc) -> Self {
        Self::new(
            AstNode::StmtIf {
                cond: Box::new(cond),
                stmt: Box::new(stmt),
                els: els.map(|els| Box::new(els)),
            },
            loc,
        )
    }
    pub fn stmt_while(cond: Ast, stmt: Ast, loc: Loc) -> Self {
        Self::new(
            AstNode::StmtWhile {
                cond: Box::new(cond),
                stmt: Box::new(stmt),
            },
            loc,
        )
    }
    pub fn stmt_for(
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
    pub fn stmt_null(loc: Loc) -> Self {
        Self::new(AstNode::StmtNull, loc)
    }
    pub fn num(n: usize, loc: Loc) -> Self {
        Self::new(AstNode::Num(n), loc)
    }
    pub fn var_ref(var: Var, loc: Loc) -> Self {
        Self::new(AstNode::VarRef(var), loc)
    }
    pub fn binop(op: BinOp, l: Ast, r: Ast, loc: Loc) -> Self {
        Self::new(
            AstNode::BinOp {
                op,
                l: Box::new(l),
                r: Box::new(r),
            },
            loc,
        )
    }
    pub fn uniop(op: UniOp, e: Ast, loc: Loc) -> Self {
        Self::new(AstNode::UniOp { op, e: Box::new(e) }, loc)
    }
    pub fn ret(e: Ast, loc: Loc) -> Self {
        Self::new(AstNode::Ret { e: Box::new(e) }, loc)
    }
    pub fn funcall(name: String, args: Vec<Ast>, ret_ty: Type, loc: Loc) -> Self {
        Self::new(AstNode::FuncCall { name, args, ret_ty }, loc)
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
    pub fn assign(loc: Loc) -> Self {
        Self::new(BinOpKind::Assign, loc)
    }
    pub fn add(loc: Loc) -> Self {
        Self::new(BinOpKind::Add, loc)
    }
    pub fn sub(loc: Loc) -> Self {
        Self::new(BinOpKind::Sub, loc)
    }
    pub fn mul(loc: Loc) -> Self {
        Self::new(BinOpKind::Mul, loc)
    }
    pub fn div(loc: Loc) -> Self {
        Self::new(BinOpKind::Div, loc)
    }
    pub fn eq(loc: Loc) -> Self {
        Self::new(BinOpKind::Eq, loc)
    }
    pub fn ne(loc: Loc) -> Self {
        Self::new(BinOpKind::Ne, loc)
    }
    pub fn lt(loc: Loc) -> Self {
        Self::new(BinOpKind::Lt, loc)
    }
    pub fn le(loc: Loc) -> Self {
        Self::new(BinOpKind::Le, loc)
    }
    pub fn gt(loc: Loc) -> Self {
        Self::new(BinOpKind::Gt, loc)
    }
    pub fn ge(loc: Loc) -> Self {
        Self::new(BinOpKind::Ge, loc)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UniOpKind {
    Positive,
    Negative,
    Addr,
    Deref,
    Sizeof,
}

pub type UniOp = Annot<UniOpKind>;

impl UniOp {
    pub fn positive(loc: Loc) -> Self {
        Self::new(UniOpKind::Positive, loc)
    }
    pub fn negative(loc: Loc) -> Self {
        Self::new(UniOpKind::Negative, loc)
    }
    pub fn addr(loc: Loc) -> Self {
        Self::new(UniOpKind::Addr, loc)
    }
    pub fn deref(loc: Loc) -> Self {
        Self::new(UniOpKind::Deref, loc)
    }
    pub fn sizeof(loc: Loc) -> Self {
        Self::new(UniOpKind::Sizeof, loc)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Scope {
    vars: HashMap<String, Var>,
}

impl Scope {
    fn new() -> Self {
        Scope {
            vars: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Env {
    scopes: Vec<Scope>,
}

impl Env {
    fn new() -> Self {
        Env {
            scopes: vec![Scope::new()],
        }
    }

    fn create_scope(&mut self) {
        self.push_scope(Scope::new());
    }
    fn push_scope(&mut self, scope: Scope) {
        self.scopes.push(scope);
    }
    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn gvars(&self) -> HashMap<String, Var> {
        self.scopes[0].vars.clone()
    }

    fn find_var(&self, name: &str) -> Option<&Var> {
        for scope in self.scopes.iter().rev() {
            if let Some(var) = scope.vars.get(name) {
                return Some(var);
            }
        }
        None
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Context {
    env: Env,
    lvars: Option<HashMap<String, Var>>,
    funcs: HashMap<String, Type>,
}

impl Context {
    fn new() -> Self {
        Context {
            env: Env::new(),
            lvars: None,
            funcs: HashMap::new(),
        }
    }

    fn add_gvar(&mut self, var: Var) -> Option<Var> {
        self.env.scopes[0].vars.insert(var.name.clone(), var)
    }
    fn add_lvar(&mut self, var: Var) -> Option<Var> {
        self.env
            .scopes
            .last_mut()
            .expect("no scopes created")
            .vars
            .insert(var.name.clone(), var.clone());
        self.lvars
            .get_or_insert_with(|| HashMap::new())
            .insert(var.name.clone(), var)
    }

    fn find_func(&self, name: &str) -> Option<&Type> {
        self.funcs.get(name)
    }
    fn add_func(&mut self, name: &str, ret_ty: Type) -> Option<Type> {
        self.funcs.insert(name.to_string(), ret_ty.clone())
    }
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

fn consume_number<T>(tokens: &mut Peekable<T>) -> Result<(usize, Loc)>
where
    T: Iterator<Item = Token>,
{
    match tokens.next() {
        Some(Token {
            value: TokenKind::Number(n),
            loc,
        }) => Ok((n, loc)),
        Some(token) => Err(ParseError::NeedTokenBefore(
            token,
            vec![TokenKind::Number(0)],
        )),
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

/// read latter half of type name (e.g. `[3][5]`)
fn read_array<T>(tokens: &mut Peekable<T>, mut ty: Type) -> Result<(Type, Loc)>
where
    T: Iterator<Item = Token>,
{
    let mut loc = Loc::NONE;
    let mut lens = Vec::new();
    while let Some(TokenKind::LBracket) = tokens.peek().map(|token| &token.value) {
        consume(tokens, TokenKind::LBracket)?;
        let (len, _loc) = consume_number(tokens)?;
        lens.push(len);
        loc = loc.merge(&consume(tokens, TokenKind::RBracket)?);
    }
    while let Some(len) = lens.pop() {
        ty = Type::array(ty, len);
    }
    Ok((ty, loc))
}

/// Parse tokens with following rules
///
/// program     = toplevel*
/// toplevel    = "int" ("*")* ident "(" ("int" declarator ("," "int" declarator)*)? ")" (";" | block)
///             | "int" ("*")* ident ("[" num "]")* ";"
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
/// declaration = "int" declarator
/// declarator  = ("*")* ident ("[" num "]")* ("=" assign)?
/// expr        = assign
/// assign      = equality ("=" assign)?
/// equality    = relational ("==" relational | "!=" relational)*
/// relational  = add ("<" add | "<=" add | ">" add | ">=" add)*
/// add         = mul ("+" mul | "-" mul)*
/// mul         = unary ("*" unary | "/" unary)*
/// unary       = postfix
///             | ("+" | "-" | "&" | "*" | "sizeof") unary
/// postfix     = primary ("[" expr "]")*
/// primary     = ident
///             | ident "(" (expr ("," expr)*)? ")"
///             | num
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
/// program     = toplevel*
fn parse_program<T>(tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_program --");

    let mut loc = Loc::NONE;
    let mut ctx = Context::new();
    let mut funcs = vec![];
    while let Some(_) = tokens.peek() {
        let func = parse_toplevel(&mut ctx, tokens)?;
        if let Some(func) = func {
            loc = loc.merge(&func.loc);
            funcs.push(func);
        }
    }
    let ret = Ok(Ast::program(funcs, ctx.env.gvars(), loc));

    debug!("parse_program: {:?}", ret);
    ret
}

/// Parse toplevel
///
/// toplevel    = "int" ("*")* ident "(" ("int" declarator ("," "int" declarator)*)? ")" (";" | block)
///             | "int" ("*")* ident ("[" num "]")* ";"
fn parse_toplevel<T>(ctx: &mut Context, tokens: &mut Peekable<T>) -> Result<Option<Ast>>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_toplevel --");

    let (ty, mut loc) = consume_type_name(tokens)?;
    let mut ty = ty.into();
    while let Some(TokenKind::Asterisk) = tokens.peek().map(|token| &token.value) {
        loc = loc.merge(&consume(tokens, TokenKind::Asterisk)?);
        ty = Type::ptr(ty);
    }
    let (name, ident_loc) = consume_ident(tokens)?;
    loc = loc.merge(&ident_loc);

    let ret = if let Some(TokenKind::LParen) = tokens.peek().map(|token| &token.value) {
        // function

        ctx.env.create_scope();
        let mut args = Vec::new();

        consume(tokens, TokenKind::LParen)?;
        loop {
            if let Some(TokenKind::RParen) = tokens.peek().map(|token| &token.value) {
                break;
            }
            if args.len() > 0 {
                consume(tokens, TokenKind::Comma)?;
            }
            let (type_name, _) = consume_type_name(tokens)?;
            // TODO: support default arguments
            let (_assign, arg, ty, d_loc) = parse_declarator(ctx, tokens, type_name.into())?;
            let ty = match ty {
                Type::Array(ty, _len) => Type::Ptr(ty),
                _ => ty,
            };
            let var = Var {
                name: arg.clone(),
                ty,
                is_local: true,
            };
            args.push(var.clone());
            if ctx.add_lvar(var).is_some() {
                return Err(ParseError::Redefinition(Token::ident(&arg, d_loc)));
            }
        }
        loc = loc.merge(&consume(tokens, TokenKind::RParen)?);

        match ctx.add_func(&name, ty.clone()) {
            Some(ret_ty) => {
                if ty != ret_ty {
                    return Err(ParseError::ConflictingTypes(Token::ident(
                        &name,
                        loc.clone(),
                    )));
                }
                // TODO: check duplicate declaration
            }
            None => {}
        };

        let ret = match tokens.peek().map(|token| &token.value) {
            Some(TokenKind::Semicolon) => {
                tokens.next().unwrap();
                Ok(None)
            }
            _ => {
                let block = parse_block(ctx, tokens)?;
                loc = loc.merge(&block.loc);
                let lvars = ctx.lvars.take().unwrap_or_else(|| HashMap::new());
                Ok(Some(Ast::func(name, args, lvars, ty, block, loc)))
            }
        };
        ctx.env.pop_scope();
        ret
    } else {
        // global var

        let (ty, _loc) = read_array(tokens, ty)?;
        consume(tokens, TokenKind::Semicolon)?;

        let var = Var {
            name: name,
            ty,
            is_local: false,
        };
        ctx.add_gvar(var);

        Ok(None)
    };

    debug!("parse_toplevel: {:?}", ret);
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
/// declaration = "int" declarator
fn parse_declaration<T>(ctx: &mut Context, tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_declaration --");

    let (type_name, loc) = consume_type_name(tokens)?;
    let (assign, name, ty, ident_loc) = parse_declarator(ctx, tokens, type_name.into())?;
    let var = Var {
        name: name.clone(),
        ty: ty.clone(),
        is_local: true,
    };
    if ctx.add_lvar(var).is_some() {
        // already declared
        return Err(ParseError::Redefinition(Token::ident(&name, ident_loc)));
    }

    let ret = match assign {
        Some(assign) => {
            let op = BinOp::assign(Loc::NONE);
            let e = Ast::var_ref(
                Var {
                    name,
                    ty,
                    is_local: true,
                },
                ident_loc,
            );
            let loc = loc.merge(&assign.loc);
            Ok(Ast::binop(op, e, assign, loc))
        }
        None => Ok(Ast::stmt_null(loc.merge(&ident_loc))),
    };

    debug!("parse_declaration: {:?}", ret);
    ret
}

/// Parse declarator
///
/// declarator  = ("*")* ident ("[" num "]")* ("=" assign)?
fn parse_declarator<T>(
    ctx: &mut Context,
    tokens: &mut Peekable<T>,
    mut ty: Type,
) -> Result<(Option<Ast>, String, Type, Loc)>
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
    loc = loc.merge(&ident_loc);

    let (ty, arr_loc) = read_array(tokens, ty)?;
    loc = loc.merge(&arr_loc);

    let assign = match tokens.peek().map(|token| &token.value) {
        Some(TokenKind::Assign) => {
            loc = loc.merge(&consume(tokens, TokenKind::Assign)?);
            Some(parse_assign(ctx, tokens)?)
        }
        _ => None,
    };

    let ret = Ok((assign, name, ty, loc));

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
/// unary       = postfix
///             | ("+" | "-" | "&" | "*" | "sizeof") unary
fn parse_unary<T>(ctx: &mut Context, tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_unary --");

    let ret = match tokens.peek().map(|token| &token.value) {
        Some(TokenKind::Plus)
        | Some(TokenKind::Minus)
        | Some(TokenKind::Amp)
        | Some(TokenKind::Asterisk)
        | Some(TokenKind::Keyword(Keyword::Sizeof)) => {
            let token = tokens.next().unwrap();
            let op = match token.value {
                TokenKind::Plus => UniOp::positive(token.loc),
                TokenKind::Minus => UniOp::negative(token.loc),
                TokenKind::Amp => UniOp::addr(token.loc),
                TokenKind::Asterisk => UniOp::deref(token.loc),
                TokenKind::Keyword(Keyword::Sizeof) => UniOp::sizeof(token.loc),
                _ => unreachable!(),
            };
            let e = parse_unary(ctx, tokens)?;
            let loc = op.loc.merge(&e.loc);
            Ok(Ast::uniop(op, e, loc))
        }
        _ => parse_postfix(ctx, tokens),
    };

    debug!("parse_unary: {:?}", ret);
    ret
}

/// Parse postfix
///
/// postfix     = primary ("[" expr "]")*
fn parse_postfix<T>(ctx: &mut Context, tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_postfix --");

    let mut postfix = parse_primary(ctx, tokens)?;
    loop {
        match tokens.peek().map(|token| &token.value) {
            Some(TokenKind::LBracket) => {
                consume(tokens, TokenKind::LBracket)?;
                let e = parse_expr(ctx, tokens)?;
                let loc = postfix.loc.merge(&consume(tokens, TokenKind::RBracket)?);
                postfix = Ast::uniop(
                    UniOp::deref(Loc::NONE),
                    Ast::binop(BinOp::add(Loc::NONE), postfix, e, loc.clone()),
                    loc,
                );
            }
            _ => break,
        }
    }
    let ret = Ok(postfix);

    debug!("parse_postfix: {:?}", ret);
    ret
}

/// Parse primary
///
/// primary     = ident
///             | ident "(" (expr ("," expr)*)? ")"
///             | num
///             | "(" expr ")"
fn parse_primary<T>(ctx: &mut Context, tokens: &mut Peekable<T>) -> Result<Ast>
where
    T: Iterator<Item = Token>,
{
    debug!("parse_primary --");

    let token = tokens.next().ok_or(ParseError::Eof)?;
    let ret = match token.value {
        TokenKind::LParen => {
            let e = parse_expr(ctx, tokens)?;
            let loc = token.loc.merge(&consume(tokens, TokenKind::RParen)?);
            Ok(Ast::new(e.value, loc))
        }
        TokenKind::Number(n) => Ok(Ast::num(n, token.loc)),
        TokenKind::Ident(name) => {
            if let Some(TokenKind::LParen) = tokens.peek().map(|token| &token.value) {
                // function call
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
                let ret_ty = ctx
                    .find_func(&name)
                    .map(|ty| ty.clone())
                    // TODO: warn users of the use of undeclared func
                    .unwrap_or(Type::Int);
                Ok(Ast::funcall(name, args, ret_ty, token.loc.merge(&loc)))
            } else {
                // TODO: support env
                let var = ctx
                    .env
                    .find_var(&name)
                    .ok_or(ParseError::Undeclared(Token::ident(
                        &name,
                        token.loc.clone(),
                    )))?;
                Ok(Ast::var_ref(var.clone(), token.loc))
            }
        }
        _ => Err(ParseError::NotExpression(token)),
    };

    debug!("parse_primary: {:?}", ret);
    ret
}
