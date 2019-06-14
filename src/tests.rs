use std::fs;
use std::path::PathBuf;
use tempdir::TempDir;

use super::*;

fn testdata_path(name: &str) -> PathBuf {
    PathBuf::from("testdata").join(name)
}

pub fn load_source(name: &str) -> String {
    fs::read_to_string(testdata_path(name)).expect("failed to load test source file")
}

fn run(testdata_name: &str) -> Result<()> {
    let tmp_dir = TempDir::new("mycc").expect("failed to create temp dir");
    let opt = {
        let input = testdata_path(testdata_name);
        let output = tmp_dir.path().join("a.s");
        Opt { input, output }
    };
    super::run(&opt)
}

#[test]
fn parse_error() {
    let names = &["step_06/invalid/missing_first_op.c"];
    for name in names {
        match run(name) {
            Err(Error::Parse(_)) => assert!(true),
            Err(err) => assert!(false, "got unexpected error: {}", err),
            Ok(()) => assert!(false, "got no error"),
        }
    }
}

#[test]
fn sema_error() {
    let names = &["step_09/sema_err/invalid_lval_1.c"];
    for name in names {
        match run(name) {
            Err(Error::Semantic(_)) => assert!(true),
            Err(err) => assert!(false, "got unexpected error: {}", err),
            Ok(()) => assert!(false, "got no error"),
        }
    }
}
