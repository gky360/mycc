use env_logger;
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

fn compile(asm_path: &Path, exec_path: &Path, compile_args: &[&str]) -> io::Result<Output> {
    let mut args = Vec::new();
    args.push("-fstack-protector");
    args.push("-o");
    args.push(exec_path.to_str().unwrap());
    args.push(asm_path.to_str().unwrap());
    args.extend_from_slice(compile_args);

    Command::new("gcc").args(args).output()
}

fn exec(exec_path: &Path, exec_args: &[&str]) -> io::Result<Output> {
    Command::new(exec_path).args(exec_args).output()
}

fn compile_and_exec(testdata_name: &str, compile_args: &[&str], exec_args: &[&str]) -> Output {
    let tmp_dir = TempDir::new("mycc").expect("failed to create temp dir");
    let exec_path = tmp_dir.path().join("a.out");
    let opt = {
        let input = testdata_path(testdata_name);
        let output = tmp_dir.path().join("a.s");
        mycc::Opt { input, output }
    };
    debug!("tmp_dir: {:?}", tmp_dir);
    debug!("exec_path: {:?}", exec_path);

    mycc::run(&opt).expect(&format!("failed to run mycc for {}", testdata_name));

    let compile_output = compile(&opt.output, &exec_path, compile_args).expect("failed to compile");
    debug!("compile_output: {:?}", compile_output);
    if !compile_output.status.success() {
        eprintln!(
            "{}",
            String::from_utf8(compile_output.stderr).expect("failed to output gcc error")
        );
        panic!("gcc exited with failure");
    }

    let exec_output = exec(&exec_path, &exec_args).expect("failed to execute");
    debug!("exec_output: {:?}", exec_output);

    exec_output
}

pub fn assert_exit_status(
    testdata_name: &str,
    compile_args: &[&str],
    exec_args: &[&str],
    status: i32,
) {
    let output = compile_and_exec(testdata_name, compile_args, exec_args);
    assert_eq!(
        output.status.code(),
        Some(status),
        "\ntestdata_name: {}\nargs: {:?}",
        testdata_name,
        exec_args
    );
}

pub fn assert_output(
    testdata_name: &str,
    compile_args: &[&str],
    exec_args: &[&str],
    status: i32,
    stdout: &str,
    stderr: &str,
) {
    let output = compile_and_exec(testdata_name, compile_args, exec_args);
    assert_eq!(
        output.status.code(),
        Some(status),
        "\ntestdata_name: {}\nargs: {:?}",
        testdata_name,
        exec_args
    );
    assert_eq!(
        &String::from_utf8(output.stdout).expect("failed to convert stdout to string"),
        stdout,
        "\ntestdata_name: {}\nargs: {:?}",
        testdata_name,
        exec_args
    );
    assert_eq!(
        &String::from_utf8(output.stderr).expect("failed to convert stderr to string"),
        stderr,
        "\ntestdata_name: {}\nargs: {:?}",
        testdata_name,
        exec_args
    );
}
