#[macro_use]
extern crate log;

use std::process;
use structopt::StructOpt;

#[cfg_attr(tarpaulin, skip)]
fn main() {
    env_logger::init();

    let result = {
        let opt = mycc::Opt::from_args();
        mycc::run(&opt)
    };

    process::exit(match result {
        Ok(()) => 0,
        Err(err) => {
            debug!("{:?}", err);
            1
        }
    })
}
