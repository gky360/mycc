use std::fmt;

use crate::asm::{Assembly, Ent, Function, Ins, Opr, Reg};
use crate::lexer::{Annot, Loc};
use crate::parser::{Ast, AstNode, BinOp, BinOpKind, UniOp, UniOpKind};

#[cfg(test)]
mod tests;

pub type Result<T> = std::result::Result<T, CompileError>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CompileErrorKind {
    LValRequired,
}

#[derive(Fail, Debug, Clone, PartialEq, Eq, Hash)]
pub struct CompileError(Annot<CompileErrorKind>);

impl CompileError {
    fn new(kind: CompileErrorKind, loc: Loc) -> Self {
        CompileError(Annot::new(kind, loc))
    }
    fn lval_required(loc: Loc) -> Self {
        CompileError::new(CompileErrorKind::LValRequired, loc)
    }

    pub fn loc(&self) -> &Loc {
        &self.0.loc
    }
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::CompileErrorKind::*;
        match self.0.value {
            LValRequired => write!(
                f,
                "{}: lvalue required as left operand of assignment",
                self.0.loc
            ),
        }
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Compiler;

impl Compiler {
    pub fn new() -> Compiler {
        Compiler
    }

    pub fn compile(&mut self, ast: &Ast) -> Result<Assembly> {
        use Opr::*;
        use Reg::*;

        let mut inss = Vec::new();

        // prolog
        inss.push(Ins::PUSH(Direct(RBP)));
        inss.push(Ins::MOV(Direct(RBP), Direct(RSP)));
        inss.push(Ins::SUB(Direct(RSP), Literal(8 * 26)));

        self.compile_ast(ast, &mut inss)?;

        // epilogue
        inss.push(Ins::MOV(Direct(RSP), Direct(RBP)));
        inss.push(Ins::POP(Direct(RBP)));
        inss.push(Ins::RET);

        let fn_main = Function::new("main", inss);
        Ok(Assembly::new(vec![
            Ent::dot("intel_syntax", "noprefix"),
            Ent::dot("global", "main"),
            Ent::Fun(fn_main),
        ]))
    }

    fn compile_lval(&mut self, ast: &Ast, inss: &mut Vec<Ins>) -> Result<()> {
        use Opr::*;
        use Reg::*;

        match &ast.value {
            AstNode::Ident(name) => {
                let offset = (b'z' - name.as_bytes()[0] + 1) as u64 * 8;
                inss.push(Ins::MOV(Direct(RAX), Direct(RBP)));
                inss.push(Ins::SUB(Direct(RAX), Literal(offset)));
                inss.push(Ins::PUSH(Direct(RAX)));
                Ok(())
            }
            _ => Err(CompileError::lval_required(ast.loc.clone())),
        }
    }

    fn compile_ast(&mut self, ast: &Ast, inss: &mut Vec<Ins>) -> Result<()> {
        use Opr::*;
        use Reg::*;

        match ast.value {
            AstNode::Num(num) => self.compile_num(num, inss)?,
            AstNode::Ident(_) => self.compile_ident(ast, inss)?,
            AstNode::BinOp {
                ref op,
                ref l,
                ref r,
            } => self.compile_binop(op, l, r, inss)?,
            AstNode::UniOp { ref op, ref e } => self.compile_uniop(op, e, inss)?,
            AstNode::Statements(ref stmts) => {
                for ast in stmts {
                    self.compile_ast(ast, inss)?;
                    inss.push(Ins::POP(Direct(RAX)));
                }
            }
        };

        Ok(())
    }

    fn compile_num(&mut self, num: u64, inss: &mut Vec<Ins>) -> Result<()> {
        use Opr::*;

        inss.push(Ins::PUSH(Literal(num)));

        Ok(())
    }

    fn compile_ident(&mut self, ast: &Ast, inss: &mut Vec<Ins>) -> Result<()> {
        use Opr::*;
        use Reg::*;

        self.compile_lval(ast, inss)?;
        inss.push(Ins::POP(Direct(RAX)));
        inss.push(Ins::MOV(Direct(RAX), Indirect(RAX)));
        inss.push(Ins::PUSH(Direct(RAX)));

        Ok(())
    }

    fn compile_binop(
        &mut self,
        binop: &BinOp,
        l: &Ast,
        r: &Ast,
        inss: &mut Vec<Ins>,
    ) -> Result<()> {
        use Opr::*;
        use Reg::*;

        if binop.value == BinOpKind::Assign {
            self.compile_lval(l, inss)?;
            self.compile_ast(r, inss)?;

            inss.push(Ins::POP(Direct(RDI)));
            inss.push(Ins::POP(Direct(RAX)));
            inss.push(Ins::MOV(Indirect(RAX), Direct(RDI)));
            inss.push(Ins::PUSH(Direct(RDI)));
            return Ok(());
        }

        self.compile_ast(l, inss)?;
        self.compile_ast(r, inss)?;
        inss.push(Ins::POP(Direct(RDI)));
        inss.push(Ins::POP(Direct(RAX)));
        match binop.value {
            BinOpKind::Assign => unreachable!(),
            BinOpKind::Add => inss.push(Ins::ADD(Direct(RAX), Direct(RDI))),
            BinOpKind::Sub => inss.push(Ins::SUB(Direct(RAX), Direct(RDI))),
            BinOpKind::Mul => inss.push(Ins::IMUL(Direct(RDI))),
            BinOpKind::Div => {
                inss.push(Ins::CQO);
                inss.push(Ins::IDIV(Direct(RDI)));
            }
            BinOpKind::Eq | BinOpKind::Ne | BinOpKind::Lt | BinOpKind::Le => {
                inss.push(Ins::CMP(Direct(RAX), Direct(RDI)));
                match binop.value {
                    BinOpKind::Eq => inss.push(Ins::SETE(Direct(AL))),
                    BinOpKind::Ne => inss.push(Ins::SETNE(Direct(AL))),
                    BinOpKind::Lt => inss.push(Ins::SETL(Direct(AL))),
                    BinOpKind::Le => inss.push(Ins::SETLE(Direct(AL))),
                    _ => {}
                }
                inss.push(Ins::MOVZB(Direct(RAX), Direct(AL)));
            }
            BinOpKind::Gt | BinOpKind::Ge => {
                inss.push(Ins::CMP(Direct(RDI), Direct(RAX)));
                match binop.value {
                    BinOpKind::Gt => inss.push(Ins::SETL(Direct(AL))),
                    BinOpKind::Ge => inss.push(Ins::SETLE(Direct(AL))),
                    _ => {}
                }
                inss.push(Ins::MOVZB(Direct(RAX), Direct(AL)));
            }
        };
        inss.push(Ins::PUSH(Direct(RAX)));

        Ok(())
    }

    fn compile_uniop(&mut self, uniop: &UniOp, e: &Ast, inss: &mut Vec<Ins>) -> Result<()> {
        self.compile_ast(e, inss)?;

        match uniop.value {
            UniOpKind::Positive => {}
            UniOpKind::Negative => {
                // consider -x as 0 - x
                inss.push(Ins::POP(Opr::Direct(Reg::RDI)));
                inss.push(Ins::MOV(Opr::Direct(Reg::RAX), Opr::Literal(0)));
                inss.push(Ins::SUB(Opr::Direct(Reg::RAX), Opr::Direct(Reg::RDI)));
                inss.push(Ins::PUSH(Opr::Direct(Reg::RAX)));
            }
        };

        Ok(())
    }
}
