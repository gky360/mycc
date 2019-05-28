#[macro_use]
extern crate log;

mod helpers;

#[test]
fn step_01_single_literal() {
    helpers::run_test(|| {
        let output = helpers::compile_and_exec("step_01/single_literal.c", &[])
            .expect("failed to compile and execute the test source file");
        assert_eq!(output.status.code(), Some(123));
    });
}
