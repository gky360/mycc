use super::*;

#[test]
fn test_display_main_only() {
    use Opr::*;
    use Reg::*;

    let inss_main = Instructions::new(vec![
        Ins::MOV(Direct(RAX), Literal(42)), // mov rax, 42
        Ins::RET,                           // ret
    ]);
    let fn_main = Function::new("main", inss_main);
    let assembly = Assembly::new(vec![
        Ent::dot("intel_syntax", "noprefix"),
        Ent::dot("global", "main"),
        Ent::Empty,
        Ent::Fun(fn_main),
    ]);

    let expected = r#"
.intel_syntax noprefix
.global main

main:
    mov rax, 42
    ret

"#;
    assert_eq!(&format!("\n{}", assembly), expected);
}

#[test]
fn test_display_with_call() {
    use Opr::*;
    use Reg::*;

    let inss_plus = Instructions::new(vec![
        Ins::ADD(Direct(RSI), Direct(RDI)), // add rsi, rdi
        Ins::MOV(Direct(RAX), Direct(RSI)), // add rsi, rdi
        Ins::RET,                           // ret
    ]);
    let fn_plus = Function::new("plus", inss_plus);

    let inss_main = Instructions::new(vec![
        Ins::MOV(Direct(RDI), Literal(3)), // mov rdi, 3
        Ins::MOV(Direct(RSI), Literal(4)), // mov rsi, 4
        Ins::call("plus"),                 // call plus
        Ins::RET,                          // ret
    ]);
    let fn_main = Function::new("main", inss_main);

    let assembly = Assembly::new(vec![
        Ent::dot("intel_syntax", "noprefix"),
        Ent::dot("global", "plus, main"),
        Ent::Empty,
        Ent::Fun(fn_plus),
        Ent::Fun(fn_main),
    ]);

    let expected = r#"
.intel_syntax noprefix
.global plus, main

plus:
    add rsi, rdi
    mov rax, rsi
    ret

main:
    mov rdi, 3
    mov rsi, 4
    call plus
    ret

"#;
    assert_eq!(&format!("\n{}", assembly), expected);
}
