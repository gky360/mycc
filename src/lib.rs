#[macro_use]
extern crate failure;
#[macro_use]
extern crate log;

use std::io::Write;
use std::path::PathBuf;
use std::{fs, io, result};
use structopt::StructOpt;

use crate::compiler::Compiler;
use crate::parser::{Ast, ParseError};

pub mod asm;
pub mod compiler;
pub mod lexer;
pub mod parser;

pub type Result<T> = result::Result<T, Error>;

#[derive(Fail, Debug)]
pub enum Error {
    #[fail(display = "IoError")]
    Io(#[fail(cause)] io::Error),
    #[fail(display = "ParseError")]
    Parse(#[fail(cause)] ParseError),
    #[fail(display = "Unknown Error")]
    Unknown,
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::Io(err)
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
    debug!("{:?}", opt);

    // read input file
    let source = fs::read_to_string(&opt.input)?;

    // parse to generate AST
    let ast: Ast = source.parse()?;

    // compile to generate assembly
    let mut compiler = Compiler::new();
    let assembly = compiler.compile(&ast);

    // output assembly to .s file
    let mut out = io::BufWriter::new(fs::File::create(&opt.output)?);
    write!(out, "{}", assembly)?;
    out.flush()?;

    Ok(())
}
