use env_logger;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::{io, panic};
use tempdir::TempDir;

pub fn run_test<T>(test: T) -> ()
where
    T: FnOnce() -> () + panic::UnwindSafe,
{
    setup();

    let result = panic::catch_unwind(|| test());

    teardown();

    assert!(result.is_ok());
}

fn setup() {
    let _ = env_logger::builder().is_test(true).try_init();
}

fn teardown() {}

fn testdata_path(name: &str) -> PathBuf {
    PathBuf::from("testdata").join(name)
}

fn compile(asm_path: &Path, exec_path: &Path) -> io::Result<Output> {
    Command::new("gcc")
        .args(&[
            "-o",
            exec_path.to_str().unwrap(),
            asm_path.to_str().unwrap(),
        ])
        .output()
}

fn exec(exec_path: &Path, args: &[&OsStr]) -> io::Result<Output> {
    Command::new(exec_path).args(args).output()
}

pub fn compile_and_exec(testdata_name: &str, args: &[&OsStr]) -> mycc::Result<Output> {
    let tmp_dir = TempDir::new("mycc").expect("failed to create temp dir");
    let exec_path = tmp_dir.path().join("a.out");
    let opt = {
        let input = testdata_path(testdata_name);
        let output = tmp_dir.path().join("a.s");
        mycc::Opt { input, output }
    };
    debug!("tmp_dir: {:?}", tmp_dir);
    debug!("exec_path: {:?}", exec_path);

    mycc::run(&opt)?;

    let compile_output = compile(&opt.output, &exec_path).expect("failed to compile");
    debug!("compile_output: {:?}", compile_output);
    if !compile_output.status.success() {
        panic!("gcc exited with failure");
    }

    let exec_output = exec(&exec_path, &args).expect("failed to execute");
    debug!("exec_output: {:?}", exec_output);

    Ok(exec_output)
}
