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

#[test]
fn test_step_09_single_char_variable() {
    let sources = vec![
        // syntax_err_bad_lvalue
        r##"
a = 2;
a + 3 = 4;
"##,
        // syntax_err_no_semicolon
        r##"
a = 2
a = a + 4;
"##,
    ];
    for source in sources {
        match Ast::from_str(source) {
            Ok(_) => assert!(false),
            Err(ParseError::Lex(_)) => assert!(false),
            Err(_) => assert!(true),
        }
    }
}