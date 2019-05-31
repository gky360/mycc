use super::*;

#[test]
fn test_step_06_invalid() {
    let sources = vec!["<= 2", "1 < > 3"];
    for source in sources {
        match Ast::from_str(source) {
            Ok(_) => assert!(false),
            Err(_) => assert!(true),
        }
    }
}