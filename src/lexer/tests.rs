use super::*;

#[test]
fn test_annotate() {
    let input = r##"
a = 2
a = a + 4;
b = 3 + a;
c = 3 + b;
"##;
    let loc = Loc(3, 21);
    let mut annotated = String::new();
    assert!(loc.annotate(&mut annotated, &input).is_ok());
    assert_eq!(
        annotated,
        r##"2 | a = 2
  |   ^^^^
3 | a = a + 4;
  | ^^^^^^^^^^^
4 | b = 3 + a;
  | ^^^
"##
    );
}

#[test]
fn test_lexer() {
    let lexer = Lexer::new("1 + 2 * 3 - -10");
    assert_eq!(
        lexer.lex(),
        Ok(vec![
            Token::number(1, Loc(0, 1)),
            Token::plus(Loc(2, 3)),
            Token::number(2, Loc(4, 5)),
            Token::asterisk(Loc(6, 7)),
            Token::number(3, Loc(8, 9)),
            Token::minus(Loc(10, 11)),
            Token::minus(Loc(12, 13)),
            Token::number(10, Loc(13, 15)),
        ])
    )
}
