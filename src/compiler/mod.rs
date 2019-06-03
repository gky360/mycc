use std::collections::HashMap;
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
    NotImplemented,
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
    fn not_implemented(loc: Loc) -> Self {
        CompileError::new(CompileErrorKind::NotImplemented, loc)
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
            NotImplemented => write!(
                f,
                "{}: compiler is not implemented for this syntax",
                self.0.loc
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Compiler<'a> {
    inss: Vec<Ins>,
    var_offset: HashMap<&'a str, u64>,
}

impl<'a> Compiler<'a> {
    pub fn new() -> Compiler<'a> {
        Compiler {
            inss: Vec::new(),
            var_offset: HashMap::new(),
        }
    }

    pub fn compile(&mut self, ast: &'a Ast) -> Result<Assembly> {
        use Opr::*;
        use Reg::*;

        self.inss.clear();
        self.var_offset.clear();

        // prolog
        self.inss.push(Ins::PUSH(Direct(RBP)));
        self.inss.push(Ins::MOV(Direct(RBP), Direct(RSP)));
        // Total size of local variables is not known at this time,
        // so mycc first reserve instruction here with `sub rsp, 0`
        // and replace `0` with actual size needed after finishing compiling the function.
        self.inss.push(Ins::SUB(Direct(RSP), Literal(0)));
        let ins_id_for_reserve_local_vars = self.inss.len() - 1;

        self.compile_ast(ast)?;
        self.inss[ins_id_for_reserve_local_vars] =
            Ins::SUB(Direct(RSP), Literal(8 * self.var_offset.len() as u64));

        // epilogue
        self.inss.push(Ins::MOV(Direct(RSP), Direct(RBP)));
        self.inss.push(Ins::POP(Direct(RBP)));
        self.inss.push(Ins::RET);

        let fn_main = Function::new("main", self.inss.clone());
        Ok(Assembly::new(vec![
            Ent::dot("intel_syntax", "noprefix"),
            Ent::dot("global", "main"),
            Ent::Fun(fn_main),
        ]))
    }

    fn compile_lval(&mut self, ast: &'a Ast) -> Result<()> {
        use Opr::*;
        use Reg::*;

        match &ast.value {
            AstNode::Ident(name) => {
                let offset = {
                    match self.var_offset.get(name as &str) {
                        Some(offset) => *offset,
                        None => {
                            let offset = 8 * (self.var_offset.len() + 1) as u64;
                            self.var_offset.insert(name, offset);
                            offset
                        }
                    }
                };
                self.inss.push(Ins::MOV(Direct(RAX), Direct(RBP)));
                self.inss.push(Ins::SUB(Direct(RAX), Literal(offset)));
                self.inss.push(Ins::PUSH(Direct(RAX)));
                Ok(())
            }
            _ => Err(CompileError::lval_required(ast.loc.clone())),
        }
    }

    fn compile_ast(&mut self, ast: &'a Ast) -> Result<()> {
        match ast.value {
            AstNode::Statements(ref stmts) => self.compile_statements(stmts),
            AstNode::StatementIf { .. } => Err(CompileError::not_implemented(ast.loc.clone())),
            AstNode::Num(num) => self.compile_num(num),
            AstNode::Ident(_) => self.compile_ident(ast),
            AstNode::BinOp {
                ref op,
                ref l,
                ref r,
            } => self.compile_binop(op, l, r),
            AstNode::UniOp { ref op, ref e } => self.compile_uniop(op, e),
            AstNode::Ret { ref e } => self.compile_ret(e),
        }
    }

    fn compile_num(&mut self, num: u64) -> Result<()> {
        use Opr::*;

        self.inss.push(Ins::PUSH(Literal(num)));

        Ok(())
    }

    fn compile_ident(&mut self, ast: &'a Ast) -> Result<()> {
        use Opr::*;
        use Reg::*;

        self.compile_lval(ast)?;
        self.inss.push(Ins::POP(Direct(RAX)));
        self.inss.push(Ins::MOV(Direct(RAX), Indirect(RAX)));
        self.inss.push(Ins::PUSH(Direct(RAX)));

        Ok(())
    }

    fn compile_binop(&mut self, binop: &BinOp, l: &'a Ast, r: &'a Ast) -> Result<()> {
        use Opr::*;
        use Reg::*;

        if binop.value == BinOpKind::Assign {
            self.compile_lval(l)?;
            self.compile_ast(r)?;

            self.inss.push(Ins::POP(Direct(RDI)));
            self.inss.push(Ins::POP(Direct(RAX)));
            self.inss.push(Ins::MOV(Indirect(RAX), Direct(RDI)));
            self.inss.push(Ins::PUSH(Direct(RDI)));
            return Ok(());
        }

        self.compile_ast(l)?;
        self.compile_ast(r)?;
        self.inss.push(Ins::POP(Direct(RDI)));
        self.inss.push(Ins::POP(Direct(RAX)));
        match binop.value {
            BinOpKind::Assign => unreachable!(),
            BinOpKind::Add => self.inss.push(Ins::ADD(Direct(RAX), Direct(RDI))),
            BinOpKind::Sub => self.inss.push(Ins::SUB(Direct(RAX), Direct(RDI))),
            BinOpKind::Mul => self.inss.push(Ins::IMUL(Direct(RDI))),
            BinOpKind::Div => {
                self.inss.push(Ins::CQO);
                self.inss.push(Ins::IDIV(Direct(RDI)));
            }
            BinOpKind::Eq | BinOpKind::Ne | BinOpKind::Lt | BinOpKind::Le => {
                self.inss.push(Ins::CMP(Direct(RAX), Direct(RDI)));
                match binop.value {
                    BinOpKind::Eq => self.inss.push(Ins::SETE(Direct(AL))),
                    BinOpKind::Ne => self.inss.push(Ins::SETNE(Direct(AL))),
                    BinOpKind::Lt => self.inss.push(Ins::SETL(Direct(AL))),
                    BinOpKind::Le => self.inss.push(Ins::SETLE(Direct(AL))),
                    _ => {}
                }
                self.inss.push(Ins::MOVZB(Direct(RAX), Direct(AL)));
            }
            BinOpKind::Gt | BinOpKind::Ge => {
                self.inss.push(Ins::CMP(Direct(RDI), Direct(RAX)));
                match binop.value {
                    BinOpKind::Gt => self.inss.push(Ins::SETL(Direct(AL))),
                    BinOpKind::Ge => self.inss.push(Ins::SETLE(Direct(AL))),
                    _ => {}
                }
                self.inss.push(Ins::MOVZB(Direct(RAX), Direct(AL)));
            }
        };
        self.inss.push(Ins::PUSH(Direct(RAX)));

        Ok(())
    }

    fn compile_uniop(&mut self, uniop: &UniOp, e: &'a Ast) -> Result<()> {
        self.compile_ast(e)?;

        match uniop.value {
            UniOpKind::Positive => {}
            UniOpKind::Negative => {
                // consider -x as 0 - x
                self.inss.push(Ins::POP(Opr::Direct(Reg::RDI)));
                self.inss
                    .push(Ins::MOV(Opr::Direct(Reg::RAX), Opr::Literal(0)));
                self.inss
                    .push(Ins::SUB(Opr::Direct(Reg::RAX), Opr::Direct(Reg::RDI)));
                self.inss.push(Ins::PUSH(Opr::Direct(Reg::RAX)));
            }
        };

        Ok(())
    }

    fn compile_ret(&mut self, e: &'a Ast) -> Result<()> {
        use Opr::*;
        use Reg::*;

        self.compile_ast(e)?;

        self.inss.push(Ins::POP(Direct(RAX)));
        self.inss.push(Ins::MOV(Direct(RSP), Direct(RBP)));
        self.inss.push(Ins::POP(Direct(RBP)));
        self.inss.push(Ins::RET);

        Ok(())
    }

    fn compile_statements(&mut self, stmts: &'a Vec<Ast>) -> Result<()> {
        use Opr::*;
        use Reg::*;

        for ast in stmts {
            self.compile_ast(ast)?;
            self.inss.push(Ins::POP(Direct(RAX)));
        }

        Ok(())
    }
}
