use std::cell::RefCell;
use std::cmp::{max, min};
use std::fmt;
use std::ops::FnMut;
use std::str::{from_utf8, FromStr};
use strum_macros::{Display, EnumString};

#[cfg(test)]
#[cfg_attr(tarpaulin, skip)]
mod tests;

pub type Result<T> = std::result::Result<T, LexError>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LexErrorKind {
    InvalidChar(char),
    Eof,
}

#[derive(Fail, Debug, Clone, PartialEq, Eq, Hash)]
pub struct LexError(Annot<LexErrorKind>);

impl LexError {
    fn new(kind: LexErrorKind, loc: Loc) -> Self {
        LexError(Annot::new(kind, loc))
    }
    fn invalid_char(c: char, loc: Loc) -> Self {
        LexError::new(LexErrorKind::InvalidChar(c), loc)
    }
    fn eof(loc: Loc) -> Self {
        LexError::new(LexErrorKind::Eof, loc)
    }

    pub fn loc(&self) -> &Loc {
        &self.0.loc
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::LexErrorKind::*;
        match &self.0.value {
            InvalidChar(c) => write!(f, "{}: invalid char '{}'", self.0.loc, c),
            Eof => write!(f, "End of file"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Loc(pub usize, pub usize);

impl Loc {
    pub const NONE: Loc = Loc(0, 0);

    pub fn merge(&self, other: &Loc) -> Loc {
        if self == &Loc::NONE {
            return other.clone();
        }
        if other == &Loc::NONE {
            return self.clone();
        }
        Loc(min(self.0, other.0), max(self.1, other.1))
    }

    pub fn annotate<T: fmt::Write>(&self, f: &mut T, input: &str) -> fmt::Result {
        // calculate required width for line numbers
        let mut c = input.lines().count();
        let mut digits = 0;
        while c > 0 {
            digits += 1;
            c /= 10;
        }

        // annotate input
        let mut sum_len = 0;
        for (i, line) in input.lines().enumerate() {
            let line_len = line.len() + 1; // take '\n' into account
            if self.0 < sum_len + line_len && sum_len < self.1 {
                writeln!(f, "{:>width$} | {}", i + 1, line, width = digits)?;
                let start = max(0, self.0 as i32 - sum_len as i32) as usize;
                writeln!(
                    f,
                    "{:>width$} | {}{}",
                    "",
                    " ".repeat(start),
                    "^".repeat(min(line_len, self.1 - sum_len) - start),
                    width = digits
                )?;
            }
            sum_len += line_len;
        }

        Ok(())
    }
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}-{}", self.0, self.1)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Annot<T> {
    pub value: T,
    pub loc: Loc,
}

impl<T> Annot<T> {
    pub fn new(value: T, loc: Loc) -> Self {
        Self { value, loc }
    }
}

#[derive(Display, EnumString, Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Keyword {
    #[strum(serialize = "else")]
    Else,
    #[strum(serialize = "for")]
    For,
    #[strum(serialize = "if")]
    If,
    #[strum(serialize = "return")]
    Return,
    #[strum(serialize = "while")]
    While,
    #[strum(serialize = "sizeof")]
    Sizeof,
}

#[derive(Display, EnumString, Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum TypeName {
    #[strum(serialize = "int")]
    Int,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TokenKind {
    /// [0-9]+
    Number(usize),
    /// identifier
    Ident(String),
    /// keyword
    Keyword(Keyword),
    /// type name
    TypeName(TypeName),
    /// +
    Plus,
    /// -
    Minus,
    /// *
    Asterisk,
    /// /
    Slash,
    /// &
    Amp,
    /// {
    LBrace,
    /// }
    RBrace,
    /// (
    LParen,
    /// )
    RParen,
    /// [
    LBracket,
    /// ]
    RBracket,
    /// ,
    Comma,
    /// ;
    Semicolon,
    /// =
    Assign,
    /// ==
    Eq,
    /// !=
    Ne,
    /// <
    Lt,
    /// <=
    Le,
    /// >
    Gt,
    /// >=
    Ge,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::TokenKind::*;
        match self {
            Number(_) => write!(f, "{{number}}"),
            Ident(name) => {
                if name == "" {
                    write!(f, "{{identifier}}")
                } else {
                    write!(f, "{}", name)
                }
            }
            Keyword(key) => write!(f, "keyword {}", key),
            TypeName(ty) => write!(f, "type name {}", ty),
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Asterisk => write!(f, "*"),
            Slash => write!(f, "/"),
            Amp => write!(f, "&"),
            LBrace => write!(f, "{{"),
            RBrace => write!(f, "}}"),
            LParen => write!(f, "("),
            RParen => write!(f, ")"),
            LBracket => write!(f, "["),
            RBracket => write!(f, "]"),
            Comma => write!(f, ","),
            Semicolon => write!(f, ";"),
            Assign => write!(f, "="),
            Eq => write!(f, "=="),
            Ne => write!(f, "!="),
            Lt => write!(f, "<"),
            Le => write!(f, "<="),
            Gt => write!(f, ">"),
            Ge => write!(f, ">="),
        }
    }
}

pub type Token = Annot<TokenKind>;

impl Token {
    pub fn number(n: usize, loc: Loc) -> Self {
        Self::new(TokenKind::Number(n), loc)
    }
    pub fn ident(name: &str, loc: Loc) -> Self {
        Self::new(TokenKind::Ident(String::from(name)), loc)
    }
    pub fn keyword(keyword: Keyword, loc: Loc) -> Self {
        Self::new(TokenKind::Keyword(keyword), loc)
    }
    pub fn type_name(ty: TypeName, loc: Loc) -> Self {
        Self::new(TokenKind::TypeName(ty), loc)
    }
    pub fn plus(loc: Loc) -> Self {
        Self::new(TokenKind::Plus, loc)
    }
    pub fn minus(loc: Loc) -> Self {
        Self::new(TokenKind::Minus, loc)
    }
    pub fn asterisk(loc: Loc) -> Self {
        Self::new(TokenKind::Asterisk, loc)
    }
    pub fn slash(loc: Loc) -> Self {
        Self::new(TokenKind::Slash, loc)
    }
    pub fn amp(loc: Loc) -> Self {
        Self::new(TokenKind::Amp, loc)
    }
    pub fn lbrace(loc: Loc) -> Self {
        Self::new(TokenKind::LBrace, loc)
    }
    pub fn rbrace(loc: Loc) -> Self {
        Self::new(TokenKind::RBrace, loc)
    }
    pub fn lparen(loc: Loc) -> Self {
        Self::new(TokenKind::LParen, loc)
    }
    pub fn rparen(loc: Loc) -> Self {
        Self::new(TokenKind::RParen, loc)
    }
    pub fn lbracket(loc: Loc) -> Self {
        Self::new(TokenKind::LBracket, loc)
    }
    pub fn rbracket(loc: Loc) -> Self {
        Self::new(TokenKind::RBracket, loc)
    }
    pub fn comma(loc: Loc) -> Self {
        Self::new(TokenKind::Comma, loc)
    }
    pub fn semicolon(loc: Loc) -> Self {
        Self::new(TokenKind::Semicolon, loc)
    }
    pub fn assign(loc: Loc) -> Self {
        Self::new(TokenKind::Assign, loc)
    }
    pub fn eq(loc: Loc) -> Self {
        Self::new(TokenKind::Eq, loc)
    }
    pub fn ne(loc: Loc) -> Self {
        Self::new(TokenKind::Ne, loc)
    }
    pub fn lt(loc: Loc) -> Self {
        Self::new(TokenKind::Lt, loc)
    }
    pub fn le(loc: Loc) -> Self {
        Self::new(TokenKind::Le, loc)
    }
    pub fn gt(loc: Loc) -> Self {
        Self::new(TokenKind::Gt, loc)
    }
    pub fn ge(loc: Loc) -> Self {
        Self::new(TokenKind::Ge, loc)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lexer<'a> {
    input: &'a [u8],
    pos: RefCell<usize>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.as_bytes(),
            pos: RefCell::new(0),
        }
    }

    pub fn lex(&self) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();

        macro_rules! lex_a_token {
            ($lexer:expr) => {{
                let tok = $lexer?;
                tokens.push(tok);
            }};
        }

        loop {
            let pos = *self.pos.borrow();
            if self.input.len() <= pos {
                break;
            }

            match self.input[pos] {
                b'0'..=b'9' => lex_a_token!(self.lex_number()),
                b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                    lex_a_token!(self.lex_keyword_or_type_name_or_ident())
                }
                b'+' => lex_a_token!(self.lex_plus()),
                b'-' => lex_a_token!(self.lex_minus()),
                b'*' => lex_a_token!(self.lex_asterisk()),
                b'/' => lex_a_token!(self.lex_slash()),
                b'&' => lex_a_token!(self.lex_amp()),
                b'{' => lex_a_token!(self.lex_lbrace()),
                b'}' => lex_a_token!(self.lex_rbrace()),
                b'(' => lex_a_token!(self.lex_lparen()),
                b')' => lex_a_token!(self.lex_rparen()),
                b'[' => lex_a_token!(self.lex_lbracket()),
                b']' => lex_a_token!(self.lex_rbracket()),
                b',' => lex_a_token!(self.lex_comma()),
                b';' => lex_a_token!(self.lex_semicolon()),
                b'=' => lex_a_token!(self.lex_assign_or_eq()),
                b'!' => lex_a_token!(self.lex_ne()),
                b'<' => lex_a_token!(self.lex_lt_or_le()),
                b'>' => lex_a_token!(self.lex_gt_or_ge()),
                b' ' | b'\n' | b'\t' => self.skip_spaces()?,
                b => return Err(LexError::invalid_char(b as char, Loc(pos, pos + 1))),
            }
        }

        Ok(tokens)
    }

    fn consume_byte(&self, b: u8) -> Result<(u8, usize)> {
        let mut pos = self.pos.borrow_mut();
        if self.input.len() <= *pos {
            return Err(LexError::eof(Loc(*pos, *pos)));
        }
        if self.input[*pos] != b {
            return Err(LexError::invalid_char(
                self.input[*pos] as char,
                Loc(*pos, *pos + 1),
            ));
        }

        *pos += 1;

        Ok((b, *pos))
    }

    fn recognize_many(&self, mut f: impl FnMut(u8) -> bool) -> usize {
        let mut pos = self.pos.borrow_mut();
        while *pos < self.input.len() && f(self.input[*pos]) {
            *pos += 1;
        }
        *pos
    }

    fn lex_number(&self) -> Result<Token> {
        let start = *self.pos.borrow();
        let end = self.recognize_many(|b| b"0123456789".contains(&b));

        let n = from_utf8(&self.input[start..end]).unwrap().parse().unwrap();
        Ok(Token::number(n, Loc(start, end)))
    }

    fn lex_keyword_or_type_name_or_ident(&self) -> Result<Token> {
        let start = *self.pos.borrow();
        let end = self.recognize_many(|b| {
            (b'a' <= b && b <= b'z')
                || (b'A' <= b && b <= b'Z')
                || (b'0' <= b && b <= b'9')
                || b == b'_'
        });
        let name = from_utf8(&self.input[start..end]).unwrap();

        // try to lex keyword or type name
        if let Ok(keyword) = Keyword::from_str(name) {
            Ok(Token::keyword(keyword, Loc(start, end)))
        } else if let Ok(ty) = TypeName::from_str(name) {
            Ok(Token::type_name(ty, Loc(start, end)))
        } else {
            Ok(Token::ident(name, Loc(start, end)))
        }
    }

    fn lex_plus(&self) -> Result<Token> {
        self.consume_byte(b'+')
            .map(|(_, end)| Token::plus(Loc(end - 1, end)))
    }
    fn lex_minus(&self) -> Result<Token> {
        self.consume_byte(b'-')
            .map(|(_, end)| Token::minus(Loc(end - 1, end)))
    }
    fn lex_asterisk(&self) -> Result<Token> {
        self.consume_byte(b'*')
            .map(|(_, end)| Token::asterisk(Loc(end - 1, end)))
    }
    fn lex_slash(&self) -> Result<Token> {
        self.consume_byte(b'/')
            .map(|(_, end)| Token::slash(Loc(end - 1, end)))
    }
    fn lex_amp(&self) -> Result<Token> {
        self.consume_byte(b'&')
            .map(|(_, end)| Token::amp(Loc(end - 1, end)))
    }
    fn lex_lbrace(&self) -> Result<Token> {
        self.consume_byte(b'{')
            .map(|(_, end)| Token::lbrace(Loc(end - 1, end)))
    }
    fn lex_rbrace(&self) -> Result<Token> {
        self.consume_byte(b'}')
            .map(|(_, end)| Token::rbrace(Loc(end - 1, end)))
    }
    fn lex_lparen(&self) -> Result<Token> {
        self.consume_byte(b'(')
            .map(|(_, end)| Token::lparen(Loc(end - 1, end)))
    }
    fn lex_rparen(&self) -> Result<Token> {
        self.consume_byte(b')')
            .map(|(_, end)| Token::rparen(Loc(end - 1, end)))
    }
    fn lex_lbracket(&self) -> Result<Token> {
        self.consume_byte(b'[')
            .map(|(_, end)| Token::lbracket(Loc(end - 1, end)))
    }
    fn lex_rbracket(&self) -> Result<Token> {
        self.consume_byte(b']')
            .map(|(_, end)| Token::rbracket(Loc(end - 1, end)))
    }
    fn lex_comma(&self) -> Result<Token> {
        self.consume_byte(b',')
            .map(|(_, end)| Token::comma(Loc(end - 1, end)))
    }
    fn lex_semicolon(&self) -> Result<Token> {
        self.consume_byte(b';')
            .map(|(_, end)| Token::semicolon(Loc(end - 1, end)))
    }
    fn lex_assign_or_eq(&self) -> Result<Token> {
        let (_, end) = self.consume_byte(b'=')?;
        match self.consume_byte(b'=') {
            Ok((_, end)) => Ok(Token::eq(Loc(end - 2, end))),
            Err(_) => Ok(Token::assign(Loc(end - 1, end))),
        }
    }
    fn lex_ne(&self) -> Result<Token> {
        self.consume_byte(b'!')?;
        self.consume_byte(b'=')
            .map(|(_, end)| Token::ne(Loc(end - 2, end)))
    }
    fn lex_lt_or_le(&self) -> Result<Token> {
        let (_, end) = self.consume_byte(b'<')?;
        match self.consume_byte(b'=') {
            Ok((_, end)) => Ok(Token::le(Loc(end - 2, end))),
            Err(_) => Ok(Token::lt(Loc(end - 1, end))),
        }
    }
    fn lex_gt_or_ge(&self) -> Result<Token> {
        let (_, end) = self.consume_byte(b'>')?;
        match self.consume_byte(b'=') {
            Ok((_, end)) => Ok(Token::ge(Loc(end - 2, end))),
            Err(_) => Ok(Token::gt(Loc(end - 1, end))),
        }
    }

    fn skip_spaces(&self) -> Result<()> {
        self.recognize_many(|b| b" \n\t".contains(&b));
        Ok(())
    }
}
