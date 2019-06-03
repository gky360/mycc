#[macro_use]
extern crate failure;
#[macro_use]
extern crate log;

use failure::Fail;
use std::io::Write;
use std::path::PathBuf;
use std::{fs, io, result};
use structopt::StructOpt;

use crate::compiler::{CompileError, Compiler};
use crate::parser::ParseError;

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
    #[fail(display = "CompileError")]
    Compile(#[fail(cause)] CompileError),
    #[fail(display = "Unknown Error")]
    Unknown,
}

impl Error {
    const INDENT: &'static str = "    ";

    pub fn show_trace(&self) {
        eprintln!("{}", self);
        let err: &Fail = self;
        for (i, err) in err.iter_causes().enumerate() {
            eprintln!("{}caused by:", Self::INDENT.repeat(i));
            eprintln!("{}{}", Self::INDENT.repeat(i + 1), err);
        }
    }
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

impl From<CompileError> for Error {
    fn from(err: CompileError) -> Error {
        Error::Compile(err)
    }
}

#[derive(StructOpt, Debug, Clone, PartialEq, Eq, Hash)]
#[structopt()]
pub struct Opt {
    /// Place the output into <output> path
    #[structopt(short = "o", parse(from_os_str), default_value = "a.s")]
    pub output: PathBuf,

    /// Load source code from <input> path
    #[structopt(parse(from_os_str))]
    pub input: PathBuf,
}

fn run_inner(opt: &Opt) -> Result<()> {
    // read input file
    let source = fs::read_to_string(&opt.input)?;

    // parse to generate AST
    let ast = source.parse().map_err(|err: ParseError| {
        err.show_diagnostic(&source);
        err
    })?;

    // compile to generate assembly
    let mut compiler = Compiler::new();
    let assembly = compiler.compile(&ast).map_err(|err| {
        err.show_diagnostic(&source);
        err
    })?;

    // output assembly to .s file
    let mut out = io::BufWriter::new(fs::File::create(&opt.output)?);
    write!(out, "{}", assembly)?;
    out.flush()?;

    Ok(())
}

pub fn run(opt: &Opt) -> Result<()> {
    debug!("{:?}", opt);

    let ret = run_inner(opt);

    match &ret {
        Err(err) => {
            err.show_trace();
        }
        Ok(_) => {}
    };
    ret
}
