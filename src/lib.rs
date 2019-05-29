#[macro_use]
extern crate failure;
#[macro_use]
extern crate log;

use std::io::Write;
use std::path::PathBuf;
use std::{fs, io, result};
use structopt::StructOpt;

use crate::asm::{Assembly, Ent, Function, Ins};
use crate::lexer::{LexError, Lexer, TokenKind};
use crate::parser::ParseError;

pub mod asm;
pub mod lexer;
pub mod parser;

pub type Result<T> = result::Result<T, Error>;

#[derive(Fail, Debug)]
pub enum Error {
    #[fail(display = "IoError: {}", _0)]
    Io(#[fail(cause)] io::Error),
    #[fail(display = "LexError: {}", _0)]
    Lex(#[fail(cause)] LexError),
    #[fail(display = "ParseError: {}", _0)]
    Parse(#[fail(cause)] ParseError),
    #[fail(display = "Unknown")]
    Unknown,
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::Io(err)
    }
}

impl From<LexError> for Error {
    fn from(err: LexError) -> Error {
        Error::Lex(err)
    }
}

impl From<ParseError> for Error {
    fn from(err: ParseError) -> Error {
        Error::Parse(err)
    }
}

#[derive(StructOpt, Debug, Clone, PartialEq, Eq, Hash)]
#[structopt()]
pub struct Opt {
    /// Input file
    #[structopt(parse(from_os_str))]
    pub input: PathBuf,

    /// Place the output into <output>
    #[structopt(short = "o", parse(from_os_str), default_value = "a.s")]
    pub output: PathBuf,
}


pub fn run(opt: &Opt) -> Result<()> {
    use asm::Opr::*;
    use asm::Reg::*;

    debug!("{:?}", opt);

    let source = fs::read_to_string(&opt.input)?;

    let lexer = Lexer::new(&source);
    let tokens = lexer.lex()?;

    let mut instructions = Vec::new();
    // if let TokenKind::Number(n) = tokens[0].value {
    //     instructions.push(Ins::MOV(Direct(RAX), Literal(n)));
    // } else {
    //     return Err(Error::Parse("unexpected token"));
    // }

    // let mut i = 1;
    // while i < tokens.len() {
    //     if let TokenKind::Number(n) = tokens[i + 1].value {
    //         let ins = match tokens[i].value {
    //             TokenKind::Plus => Ins::ADD(Direct(RAX), Literal(n)),
    //             TokenKind::Minus => Ins::SUB(Direct(RAX), Literal(n)),
    //             _ => return Err(Error::Parse("unexpected token")),
    //         };
    //         instructions.push(ins);
    //     } else {
    //         return Err(Error::Parse("unexpected token"));
    //     }
    //     i += 2;
    // }
    // if i != tokens.len() {
    //     return Err(Error::Parse("unexpected token"));
    // }

    instructions.push(Ins::RET);

    let fn_main = Function::new("main", instructions);
    let assembly = Assembly::new(vec![
        Ent::dot("intel_syntax", "noprefix"),
        Ent::dot("global", "main"),
        Ent::Empty,
        Ent::Fun(fn_main),
    ]);

    let mut out = io::BufWriter::new(fs::File::create(&opt.output)?);
    write!(out, "{}", assembly)?;
    out.flush()?;

    Ok(())
}
