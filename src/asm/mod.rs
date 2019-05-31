use std::fmt;

static INDENT: &str = "    ";

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Assembly(Vec<Ent>);

impl Assembly {
    pub fn new(entries: Vec<Ent>) -> Assembly {
        Assembly(entries)
    }
}

impl fmt::Display for Assembly {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for ent in self.0.iter() {
            writeln!(f, "{}", ent)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Ent {
    /// e.g. `.intel_syntax noprefix`
    Dot(String, String),
    /// e.g. `main:`
    Fun(Function),
    /// empty line
    Empty,
    Raw(String),
}

impl Ent {
    pub fn dot(name: &str, content: &str) -> Ent {
        Ent::Dot(String::from(name), String::from(content))
    }

    pub fn raw(content: &str) -> Ent {
        Ent::Raw(String::from(content))
    }
}

impl fmt::Display for Ent {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Ent::*;
        match self {
            Dot(name, content) => write!(f, ".{} {}", name, content),
            Fun(fun) => write!(f, "{}", fun),
            Empty => write!(f, ""),
            Raw(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Function(String, Vec<Ins>);

impl Function {
    pub fn new(name: &str, instructions: Vec<Ins>) -> Function {
        Function(String::from(name), instructions)
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}:", self.0)?;
        for ins in self.1.iter() {
            writeln!(f, "{}{}", INDENT, ins)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Ins {
    ADD(Opr, Opr),
    CALL(String),
    CMP(Opr, Opr),
    CQO,
    IDIV(Opr),
    IMUL(Opr),
    MOV(Opr, Opr),
    MOVZB(Opr, Opr),
    POP(Opr),
    PUSH(Opr),
    SETE(Opr),
    SETL(Opr),
    SETLE(Opr),
    SETNE(Opr),
    SUB(Opr, Opr),
    RET,
}

impl Ins {
    pub fn call(name: &str) -> Ins {
        Ins::CALL(String::from(name))
    }
}

impl fmt::Display for Ins {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Ins::*;
        match self {
            ADD(opr1, opr2) => write!(f, "add {}, {}", opr1, opr2),
            CQO => write!(f, "cqo"),
            CALL(name) => write!(f, "call {}", name),
            CMP(opr1, opr2) => write!(f, "cmp {}, {}", opr1, opr2),
            IDIV(opr) => write!(f, "idiv {}", opr),
            IMUL(opr) => write!(f, "imul {}", opr),
            MOV(opr1, opr2) => write!(f, "mov {}, {}", opr1, opr2),
            MOVZB(opr1, opr2) => write!(f, "movzb {}, {}", opr1, opr2),
            POP(opr) => write!(f, "pop {}", opr),
            PUSH(opr) => write!(f, "push {}", opr),
            SETE(opr) => write!(f, "sete {}", opr),
            SETL(opr) => write!(f, "setl {}", opr),
            SETLE(opr) => write!(f, "setle {}", opr),
            SETNE(opr) => write!(f, "setne {}", opr),
            SUB(opr1, opr2) => write!(f, "sub {}, {}", opr1, opr2),
            RET => write!(f, "ret"),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Opr {
    Direct(Reg),
    Indirect(Reg),
    Literal(u64),
}

impl fmt::Display for Opr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Opr::*;
        match self {
            Direct(reg) => write!(f, "{}", reg),
            Indirect(reg) => write!(f, "[{}]", reg),
            Literal(l) => write!(f, "{}", l),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum SegReg {
    CS,
    DS,
    ES,
    FS,
    GS,
    SS,
}

impl fmt::Display for SegReg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use SegReg::*;
        match self {
            CS => write!(f, "cs"),
            DS => write!(f, "ds"),
            ES => write!(f, "es"),
            FS => write!(f, "fs"),
            GS => write!(f, "gs"),
            SS => write!(f, "ss"),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Reg {
    AL,
    RAX,
    RBX,
    RCX,
    RDX,
    RBP,
    RSP,
    RSI,
    RDI,
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Reg::*;
        match self {
            AL => write!(f, "al"),
            RAX => write!(f, "rax"),
            RBX => write!(f, "rbx"),
            RCX => write!(f, "rcx"),
            RDX => write!(f, "rdx"),
            RBP => write!(f, "rbp"),
            RSP => write!(f, "rsp"),
            RSI => write!(f, "rsi"),
            RDI => write!(f, "rdi"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display_main_only() {
        use Opr::*;
        use Reg::*;

        let fn_main = Function::new(
            "main",
            vec![
                Ins::MOV(Direct(RAX), Literal(42)), // mov rax, 42
                Ins::RET,                           // ret
            ],
        );
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

        let fn_plus = Function::new(
            "plus",
            vec![
                Ins::ADD(Direct(RSI), Direct(RDI)), // add rsi, rdi
                Ins::MOV(Direct(RAX), Direct(RSI)), // add rsi, rdi
                Ins::RET,                           // ret
            ],
        );
        let fn_main = Function::new(
            "main",
            vec![
                Ins::MOV(Direct(RDI), Literal(3)), // mov rdi, 3
                Ins::MOV(Direct(RSI), Literal(4)), // mov rsi, 4
                Ins::call("plus"),                 // call plus
                Ins::RET,                          // ret
            ],
        );
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
}
