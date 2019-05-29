#[macro_use]
extern crate failure;
#[macro_use]
extern crate log;

use std::io::Write;
use std::path::PathBuf;
use std::{fs, io, result};
use structopt::StructOpt;

use crate::asm::{Assembly, Ent, Function, Ins};

pub mod asm;

pub type Result<T> = result::Result<T, Error>;

#[derive(Fail, Debug)]
pub enum Error {
    #[fail(display = "IoError: {}", _0)]
    Io(io::Error),
    #[fail(display = "ParseError: {}", _0)]
    Parse(&'static str),
    #[fail(display = "Unknown")]
    Unknown,
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::Io(err)
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

    let mut instructions = Vec::new();
    let mut literal = String::new();
    let mut prev_op = ' ';
    for c in source.chars() {
        match c {
            '+' | '-' | '\n' => {
                let l = literal
                    .parse()
                    .map_err(|_| Error::Parse("failed to parse literal"))?;
                let ins = if prev_op == '+' {
                    Ins::ADD(Direct(RAX), Literal(l))
                } else if prev_op == '-' {
                    Ins::SUB(Direct(RAX), Literal(l))
                } else {
                    Ins::MOV(Direct(RAX), Literal(l))
                };
                instructions.push(ins);
                literal.clear();
                prev_op = c;
            }
            c if c.is_digit(10) => literal.push(c),
            _ => return Err(Error::Parse("unexpected token")),
        }
    }
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