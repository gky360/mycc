use std::{fmt, ops};
use strum_macros::Display;

#[cfg(test)]
#[cfg_attr(tarpaulin, skip)]
mod tests;

static INDENT: &str = "    ";

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Assembly<'a>(Vec<Ent<'a>>);

impl<'a> Assembly<'a> {
    pub fn new(entries: Vec<Ent<'a>>) -> Assembly {
        Assembly(entries)
    }
}

impl<'a> fmt::Display for Assembly<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for ent in self.0.iter() {
            writeln!(f, "{}", ent)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Ent<'a> {
    /// e.g. `.intel_syntax noprefix`
    Dot(&'a str, &'a str),
    /// e.g. `main:`
    Fun(Function<'a>),
    /// empty line
    Empty,
    Raw(String),
}

impl<'a> Ent<'a> {
    pub fn dot(name: &'a str, content: &'a str) -> Ent<'a> {
        Ent::Dot(name, content)
    }

    pub fn raw(content: &str) -> Ent {
        Ent::Raw(String::from(content))
    }
}

impl<'a> fmt::Display for Ent<'a> {
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
pub struct Function<'a> {
    name: String,
    instructions: &'a Instructions,
}

impl<'a> Function<'a> {
    pub fn new(name: &str, instructions: &'a Instructions) -> Self {
        Function {
            name: String::from(name),
            instructions,
        }
    }
}

impl<'a> fmt::Display for Function<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}:", self.name)?;
        write!(f, "{}", self.instructions)
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Instructions {
    pub data: Vec<Ins>,
    pub stackpos: i32,
}

impl Instructions {
    pub fn new(instructions: Vec<Ins>) -> Self {
        Instructions {
            data: instructions,
            stackpos: 0,
        }
    }

    pub fn clear(&mut self) {
        self.data.clear();
        self.stackpos = 0;
    }

    pub fn push(&mut self, ins: Ins) {
        match ins {
            Ins::PUSH(_) => {
                self.stackpos += 8;
            }
            Ins::POP(_) => {
                // TODO: check if stackops >= 0
                // assert!(self.stackpos >= 8, "tried to pop empty stack");
                self.stackpos -= 8;
            }
            _ => {}
        }
        self.data.push(ins);
    }

    pub fn len(&self) -> usize {
        self.data.len()
    }
}

impl ops::Index<usize> for Instructions {
    type Output = Ins;

    fn index(&self, idx: usize) -> &Self::Output {
        &self.data[idx]
    }
}

impl ops::IndexMut<usize> for Instructions {
    fn index_mut<'a>(&'a mut self, index: usize) -> &'a mut Self::Output {
        &mut self.data[index]
    }
}

impl fmt::Display for Instructions {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for ins in self.data.iter() {
            match ins {
                Ins::DefLabel(label) => writeln!(f, "{}:", label)?,
                ins => writeln!(f, "{}{}", INDENT, ins)?,
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Ins {
    /// define label
    /// e.g. `.Lend_xxx:`
    DefLabel(Label),

    ADD(Opr, Opr),
    CALL(String),
    CMP(Opr, Opr),
    CQO,
    IDIV(Opr),
    IMUL(Opr),
    JE(Label),
    JMP(Label),
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
            DefLabel(label) => write!(f, "{}", label),

            ADD(opr1, opr2) => write!(f, "add {}, {}", opr1, opr2),
            CQO => write!(f, "cqo"),
            CALL(name) => write!(f, "call {}", name),
            CMP(opr1, opr2) => write!(f, "cmp {}, {}", opr1, opr2),
            IDIV(opr) => write!(f, "idiv {}", opr),
            IMUL(opr) => write!(f, "imul {}", opr),
            JE(label) => write!(f, "je {}", label),
            JMP(label) => write!(f, "jmp {}", label),
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
pub struct Label {
    name: &'static str,
    id: usize,
}

impl Label {
    pub fn new(name: &'static str, id: usize) -> Self {
        Label { name, id }
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, ".L{}_{:>08x}", self.name, self.id)
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

#[derive(Display, Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum SegReg {
    #[strum(serialize = "cs")]
    CS,
    #[strum(serialize = "ds")]
    DS,
    #[strum(serialize = "es")]
    ES,
    #[strum(serialize = "fs")]
    FS,
    #[strum(serialize = "gs")]
    GS,
    #[strum(serialize = "ss")]
    SS,
}

#[derive(Display, Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Reg {
    #[strum(serialize = "al")]
    AL,
    #[strum(serialize = "rax")]
    RAX,
    #[strum(serialize = "rbx")]
    RBX,
    #[strum(serialize = "rcx")]
    RCX,
    #[strum(serialize = "rdx")]
    RDX,
    #[strum(serialize = "rbp")]
    RBP,
    #[strum(serialize = "rsp")]
    RSP,
    #[strum(serialize = "rsi")]
    RSI,
    #[strum(serialize = "rdi")]
    RDI,
    #[strum(serialize = "r8")]
    R8,
    #[strum(serialize = "r9")]
    R9,
    #[strum(serialize = "r10")]
    R10,
}
