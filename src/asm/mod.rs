use std::fmt;

static INDENT: &str = "    ";

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Assembly(Vec<Ent>);

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
    fn dot(name: &str, content: &str) -> Ent {
        Ent::Dot(String::from(name), String::from(content))
    }

    fn raw(content: &str) -> Ent {
        Ent::Raw(String::from(content))
    }
}

impl fmt::Display for Ent {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Ent::*;
        match self {
            Dot(name, content) => write!(f, ".{} {}", name, content),
            Fun(fun) => write!(f, "{}", fun),
            Emp => write!(f, ""),
            Raw(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Function(String, Vec<Ins>);

impl Function {
    fn new(name: &str, instructions: Vec<Ins>) -> Function {
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
    MOV(Opr, Opr),
    RET,
}

impl Ins {
    fn call(name: &str) -> Ins {
        Ins::CALL(String::from(name))
    }
}

impl fmt::Display for Ins {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Ins::*;
        match self {
            ADD(opr1, opr2) => write!(f, "add {}, {}", opr1, opr2),
            CALL(name) => write!(f, "call {}", name),
            MOV(opr1, opr2) => write!(f, "mov {}, {}", opr1, opr2),
            RET => write!(f, "ret"),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Opr {
    Direct(Reg),
    Literal(u64),
}

impl fmt::Display for Opr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Opr::*;
        match self {
            Direct(reg) => write!(f, "{}", reg),
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
    RAX,
    RBX,
    RCX,
    RDX,
    RSI,
    RDI,
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Reg::*;
        match self {
            RAX => write!(f, "rax"),
            RBX => write!(f, "rbx"),
            RCX => write!(f, "rcx"),
            RDX => write!(f, "rdx"),
            RSI => write!(f, "rsi"),
            RDI => write!(f, "rdi"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display() {
        use Ins::*;
        use Opr::*;
        use Reg::*;

        let fn_main = Function::new(
            "main",
            vec![
                MOV(Direct(RAX), Literal(42)), // mov rax, 42
                RET,                           // ret
            ],
        );
        let assembly = Assembly(vec![
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
}
