#[macro_use]
extern crate log;

use std::process;
use structopt::StructOpt;

fn main() {
    env_logger::init();

    let result = {
        let opt = mycc::Opt::from_args();
        mycc::run(&opt)
    };

    process::exit(match result {
        Ok(()) => 0,
        Err(err) => {
            eprintln!("{}", err);
            debug!("{:?}", err);
            1
        }
    })
}
