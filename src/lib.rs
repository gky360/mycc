#[macro_use]
extern crate failure;
#[macro_use]
extern crate log;

use failure::Fail;
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

impl Error {
    pub fn show_trace(&self) {
        let indent = "    ";
        eprintln!("{}", self);
        let err: &Fail = self;
        for (i, err) in err.iter_causes().enumerate() {
            eprintln!("{}caused by:", indent.repeat(i));
            eprintln!("{}{}", indent.repeat(i + 1), err);
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
    let ast: Ast = match source.parse() {
        Err(err) => {
            (&err as &ParseError).show_diagnostic(&source);
            return Err(Error::Parse(err));
        }
        Ok(ast) => ast,
    };

    // compile to generate assembly
    let mut compiler = Compiler::new();
    let assembly = compiler.compile(&ast);

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
        },
        Ok(_)=>{}
    };
    ret
}
