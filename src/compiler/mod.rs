use std::collections::HashMap;
use std::fmt;

use crate::asm::{Assembly, Ent, Function, Ins, Instructions, Label, Opr, Reg};
use crate::lexer::{Annot, Loc};
use crate::parser::{Ast, AstNode, BinOp, BinOpKind, UniOp, UniOpKind};

#[cfg(test)]
#[cfg_attr(tarpaulin, skip)]
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

    #[allow(dead_code)]
    fn not_implemented(loc: Loc) -> Self {
        CompileError::new(CompileErrorKind::NotImplemented, loc)
    }

    pub fn loc(&self) -> &Loc {
        &self.0.loc
    }

    pub fn show_diagnostic(&self, input: &str) {
        let mut message = String::new();
        self.loc()
            .annotate(&mut message, input)
            .expect("failed to generate error message.");
        eprintln!("{}", message);
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
    inss: Instructions,
    var_offset: HashMap<&'a str, u64>,
    next_label_id: usize,
}

impl<'a> Compiler<'a> {
    const ARG_REGS: &'a [Reg] = &[Reg::RDI, Reg::RSI, Reg::RDX, Reg::RCX, Reg::R8, Reg::R9];

    pub fn new() -> Compiler<'a> {
        Compiler {
            inss: Instructions::new(Vec::new()),
            var_offset: HashMap::new(),
            next_label_id: 0,
        }
    }

    fn increment_label_id(&mut self) -> usize {
        let label_id = self.next_label_id;
        self.next_label_id += 1;
        label_id
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
        let local_area = 8 * self.var_offset.len();
        self.inss[ins_id_for_reserve_local_vars] =
            Ins::SUB(Direct(RSP), Literal(local_area as u64));
        self.inss.stackpos += local_area as i32;

        // epilogue
        self.inss.push(Ins::MOV(Direct(RSP), Direct(RBP)));
        self.inss.push(Ins::POP(Direct(RBP)));
        self.inss.push(Ins::RET);

        let fn_main = Function::new("main", &self.inss);
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
            AstNode::Block(ref stmts) => self.compile_block(stmts),
            AstNode::StmtIf {
                ref cond,
                ref stmt,
                ref els,
            } => self.compile_stmt_if(cond, stmt, els),
            AstNode::StmtWhile { ref cond, ref stmt } => self.compile_stmt_while(cond, stmt),
            AstNode::StmtFor {
                ref init,
                ref cond,
                ref incr,
                ref stmt,
            } => self.compile_stmt_for(init, cond, incr, stmt),
            AstNode::Num(num) => self.compile_num(num),
            AstNode::Ident(_) => self.compile_ident(ast),
            AstNode::BinOp {
                ref op,
                ref l,
                ref r,
            } => self.compile_binop(op, l, r),
            AstNode::UniOp { ref op, ref e } => self.compile_uniop(op, e),
            AstNode::Ret { ref e } => self.compile_ret(e),
            AstNode::FuncCall { ref name, ref args } => self.compile_func_call(name, args),
        }
    }

    fn compile_block(&mut self, stmts: &'a Vec<Ast>) -> Result<()> {
        use Opr::*;
        use Reg::*;

        for ast in stmts {
            self.compile_ast(ast)?;
            self.inss.push(Ins::POP(Direct(RAX)));
        }
        self.inss.push(Ins::PUSH(Direct(RAX)));

        Ok(())
    }

    fn compile_stmt_if(
        &mut self,
        cond: &'a Ast,
        stmt: &'a Ast,
        els: &'a Option<Box<Ast>>,
    ) -> Result<()> {
        use Opr::*;
        use Reg::*;

        self.compile_ast(cond)?;

        let id = self.increment_label_id();
        self.inss.push(Ins::POP(Direct(RAX)));
        self.inss.push(Ins::CMP(Direct(RAX), Literal(0)));

        let label_end = Label::new("end", id);
        match els {
            None => {
                self.inss.push(Ins::JE(label_end));
                self.compile_ast(stmt)?;
                self.inss.push(Ins::POP(Direct(RAX)));
            }
            Some(els) => {
                let label_else = Label::new("else", id);
                self.inss.push(Ins::JE(label_else));
                self.compile_ast(stmt)?;
                self.inss.push(Ins::POP(Direct(RAX)));
                self.inss.push(Ins::JMP(label_end));

                self.inss.push(Ins::DefLabel(label_else));
                self.compile_ast(els)?;
                self.inss.push(Ins::POP(Direct(RAX)));
            }
        }
        self.inss.push(Ins::DefLabel(label_end));
        self.inss.push(Ins::PUSH(Direct(RAX)));

        Ok(())
    }

    fn compile_stmt_while(&mut self, cond: &'a Ast, stmt: &'a Ast) -> Result<()> {
        use Opr::*;
        use Reg::*;

        let id = self.increment_label_id();
        let label_begin = Label::new("begin", id);
        let label_end = Label::new("end", id);

        self.inss.push(Ins::DefLabel(label_begin));
        self.compile_ast(cond)?;
        self.inss.push(Ins::POP(Direct(RAX)));
        self.inss.push(Ins::CMP(Direct(RAX), Literal(0)));
        self.inss.push(Ins::JE(label_end));
        self.compile_ast(stmt)?;
        self.inss.push(Ins::POP(Direct(RAX)));
        self.inss.push(Ins::JMP(label_begin));
        self.inss.push(Ins::DefLabel(label_end));
        self.inss.push(Ins::PUSH(Direct(RAX)));

        Ok(())
    }

    fn compile_stmt_for(
        &mut self,
        init: &'a Option<Box<Ast>>,
        cond: &'a Option<Box<Ast>>,
        incr: &'a Option<Box<Ast>>,
        stmt: &'a Ast,
    ) -> Result<()> {
        use Opr::*;
        use Reg::*;

        let id = self.increment_label_id();
        let label_begin = Label::new("begin", id);
        let label_end = Label::new("end", id);

        if let Some(init) = init {
            self.compile_ast(init)?;
            self.inss.push(Ins::POP(Direct(RAX)));
        }
        self.inss.push(Ins::DefLabel(label_begin));
        if let Some(cond) = cond {
            self.compile_ast(cond)?;
            self.inss.push(Ins::POP(Direct(RAX)));
            self.inss.push(Ins::CMP(Direct(RAX), Literal(0)));
            self.inss.push(Ins::JE(label_end));
        }
        self.compile_ast(stmt)?;
        if let Some(incr) = incr {
            self.compile_ast(incr)?;
            self.inss.push(Ins::POP(Direct(RAX)));
        }
        self.inss.push(Ins::JMP(label_begin));
        self.inss.push(Ins::DefLabel(label_end));
        self.inss.push(Ins::PUSH(Direct(RAX)));

        Ok(())
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

            self.inss.push(Ins::POP(Direct(R10)));
            self.inss.push(Ins::POP(Direct(RAX)));
            self.inss.push(Ins::MOV(Indirect(RAX), Direct(R10)));
            self.inss.push(Ins::PUSH(Direct(R10)));
            return Ok(());
        }

        self.compile_ast(l)?;
        self.compile_ast(r)?;
        self.inss.push(Ins::POP(Direct(R10)));
        self.inss.push(Ins::POP(Direct(RAX)));
        match binop.value {
            BinOpKind::Assign => unreachable!(),
            BinOpKind::Add => self.inss.push(Ins::ADD(Direct(RAX), Direct(R10))),
            BinOpKind::Sub => self.inss.push(Ins::SUB(Direct(RAX), Direct(R10))),
            BinOpKind::Mul => self.inss.push(Ins::IMUL(Direct(R10))),
            BinOpKind::Div => {
                self.inss.push(Ins::CQO);
                self.inss.push(Ins::IDIV(Direct(R10)));
            }
            BinOpKind::Eq | BinOpKind::Ne | BinOpKind::Lt | BinOpKind::Le => {
                self.inss.push(Ins::CMP(Direct(RAX), Direct(R10)));
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
                self.inss.push(Ins::CMP(Direct(R10), Direct(RAX)));
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
                self.inss.push(Ins::POP(Opr::Direct(Reg::R10)));
                self.inss
                    .push(Ins::MOV(Opr::Direct(Reg::RAX), Opr::Literal(0)));
                self.inss
                    .push(Ins::SUB(Opr::Direct(Reg::RAX), Opr::Direct(Reg::R10)));
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

    fn compile_func_call(&mut self, name: &str, args: &'a Vec<Ast>) -> Result<()> {
        use Opr::*;
        use Reg::*;

        let org_stackpos = self.inss.stackpos;

        for (i, arg) in args.iter().enumerate() {
            self.compile_ast(arg)?;
            self.inss.push(Ins::POP(Direct(Self::ARG_REGS[i])));
        }

        let need_padding = self.inss.stackpos % 16 != 0;
        if need_padding {
            self.inss.push(Ins::SUB(Direct(RSP), Literal(8)));
            self.inss.stackpos += 8;
        }

        self.inss.push(Ins::CALL(String::from(name)));

        if need_padding {
            self.inss.push(Ins::ADD(Direct(RSP), Literal(8)));
            self.inss.stackpos -= 8;
        }

        assert_eq!(self.inss.stackpos, org_stackpos);
        Ok(())
    }
}
