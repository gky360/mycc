#[macro_use]
extern crate failure;
#[macro_use]
extern crate log;

use std::result;
use structopt::StructOpt;

pub type Result<T> = result::Result<T, Error>;

#[derive(Fail, Debug)]
pub enum Error {
    #[fail(display = "Unknown")]
    Unknown,
}

#[derive(StructOpt, Debug, Clone, PartialEq, Eq, Hash)]
#[structopt()]
pub struct Opt {
    hoge: i32,
}

pub fn run(opt: &Opt) -> Result<()> {
    debug!("{:?}", opt);

    Ok(())
}
