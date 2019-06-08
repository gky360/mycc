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
struct Context {
    inss: Instructions,
    var_offset: HashMap<String, u64>,
    ret_label: Label,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Compiler {
    next_label_id: usize,
}

impl Compiler {
    const ARG_REGS: &'static [Reg] = &[Reg::RDI, Reg::RSI, Reg::RDX, Reg::RCX, Reg::R8, Reg::R9];

    pub fn new() -> Compiler {
        Compiler { next_label_id: 0 }
    }

    fn increment_label_id(&mut self) -> usize {
        let label_id = self.next_label_id;
        self.next_label_id += 1;
        label_id
    }

    pub fn compile(&mut self, ast: &Ast) -> Result<Assembly> {
        let mut assembly = Assembly::new(vec![Ent::dot("intel_syntax", "noprefix")]);

        let funcs = match &ast.value {
            AstNode::Program { funcs } => funcs,
            _ => unreachable!("root ast node should be Program"),
        };
        self.compile_program(funcs, &mut assembly)?;

        Ok(assembly)
    }

    fn compile_program(&mut self, funcs: &Vec<Ast>, assembly: &mut Assembly) -> Result<()> {
        for func in funcs {
            let (name, args, body) = match &func.value {
                AstNode::Func { name, args, body } => (name, args, body),
                _ => unreachable!("ast node under Program should be Func"),
            };
            assembly.push(Ent::dot("global", name));
            assembly.push(Ent::Fun(self.compile_func(name, args, body)?));
        }

        Ok(())
    }

    fn compile_func(&mut self, name: &str, args: &Vec<String>, body: &Ast) -> Result<Function> {
        use Opr::*;
        use Reg::*;

        let id = self.increment_label_id();
        let mut ctx = Context {
            inss: Instructions::new(vec![]),
            var_offset: HashMap::new(),
            ret_label: Label::new("end", id),
        };

        // prolog
        ctx.inss.push(Ins::PUSH(Direct(RBP)));
        ctx.inss.push(Ins::MOV(Direct(RBP), Direct(RSP)));
        // Total size of local variables is not known at this time,
        // so mycc first reserve instruction here with `sub rsp, 0`
        // and replace `0` with actual size needed after finishing compiling the function.
        ctx.inss.push(Ins::SUB(Direct(RSP), Literal(0)));
        let ins_id_for_reserve_local_vars = ctx.inss.len() - 1;

        for (i, arg) in args.iter().enumerate() {
            let offset = 8 * (ctx.var_offset.len() + 1) as u64;
            ctx.var_offset.insert(arg.clone(), offset);
            ctx.inss.push(Ins::MOV(Direct(RAX), Direct(RBP)));
            ctx.inss.push(Ins::SUB(Direct(RAX), Literal(offset)));
            ctx.inss
                .push(Ins::MOV(Indirect(RAX), Direct(Self::ARG_REGS[i])));
        }

        self.compile_ast(&mut ctx, body)?;
        let local_area = 8 * ctx.var_offset.len();
        ctx.inss[ins_id_for_reserve_local_vars] = Ins::SUB(Direct(RSP), Literal(local_area as u64));
        ctx.inss.stackpos += local_area as i32;

        // epilogue
        ctx.inss.push(Ins::DefLabel(ctx.ret_label));
        ctx.inss.push(Ins::MOV(Direct(RSP), Direct(RBP)));
        ctx.inss.push(Ins::POP(Direct(RBP)));
        ctx.inss.push(Ins::RET);

        Ok(Function::new(name, ctx.inss))
    }

    fn compile_lval(&mut self, ctx: &mut Context, ast: &Ast) -> Result<()> {
        use Opr::*;
        use Reg::*;

        match &ast.value {
            AstNode::Ident(name) => {
                let offset = {
                    match ctx.var_offset.get(name) {
                        Some(offset) => *offset,
                        None => {
                            let offset = 8 * (ctx.var_offset.len() + 1) as u64;
                            ctx.var_offset.insert(name.clone(), offset);
                            offset
                        }
                    }
                };
                ctx.inss.push(Ins::MOV(Direct(RAX), Direct(RBP)));
                ctx.inss.push(Ins::SUB(Direct(RAX), Literal(offset)));
                ctx.inss.push(Ins::PUSH(Direct(RAX)));
                Ok(())
            }
            _ => Err(CompileError::lval_required(ast.loc.clone())),
        }
    }

    fn compile_ast(&mut self, ctx: &mut Context, ast: &Ast) -> Result<()> {
        match ast.value {
            // AstNode::Program { ref funcs } => self.compile_program(funcs),
            // AstNode::Func {
            //     ref name,
            //     ref args,
            //     ref body,
            // } => self.compile_func(name, args, body),
            AstNode::Block(ref stmts) => self.compile_block(ctx, stmts),
            AstNode::StmtIf {
                ref cond,
                ref stmt,
                ref els,
            } => self.compile_stmt_if(ctx, cond, stmt, els),
            AstNode::StmtWhile { ref cond, ref stmt } => self.compile_stmt_while(ctx, cond, stmt),
            AstNode::StmtFor {
                ref init,
                ref cond,
                ref incr,
                ref stmt,
            } => self.compile_stmt_for(ctx, init, cond, incr, stmt),
            AstNode::Num(num) => self.compile_num(ctx, num),
            AstNode::Ident(_) => self.compile_ident(ctx, ast),
            AstNode::BinOp {
                ref op,
                ref l,
                ref r,
            } => self.compile_binop(ctx, op, l, r),
            AstNode::UniOp { ref op, ref e } => self.compile_uniop(ctx, op, e),
            AstNode::Ret { ref e } => self.compile_ret(ctx, e),
            AstNode::FuncCall { ref name, ref args } => self.compile_func_call(ctx, name, args),
            _ => unreachable!("invalid ast structure"),
        }
    }

    fn compile_block(&mut self, ctx: &mut Context, stmts: &Vec<Ast>) -> Result<()> {
        use Opr::*;
        use Reg::*;

        for ast in stmts {
            self.compile_ast(ctx, ast)?;
            ctx.inss.push(Ins::POP(Direct(RAX)));
        }
        ctx.inss.push(Ins::PUSH(Direct(RAX)));

        Ok(())
    }

    fn compile_stmt_if(
        &mut self,
        ctx: &mut Context,
        cond: &Ast,
        stmt: &Ast,
        els: &Option<Box<Ast>>,
    ) -> Result<()> {
        use Opr::*;
        use Reg::*;

        self.compile_ast(ctx, cond)?;

        let id = self.increment_label_id();
        ctx.inss.push(Ins::POP(Direct(RAX)));
        ctx.inss.push(Ins::CMP(Direct(RAX), Literal(0)));

        let label_end = Label::new("end", id);
        match els {
            None => {
                ctx.inss.push(Ins::JE(label_end));
                self.compile_ast(ctx, stmt)?;
                ctx.inss.push(Ins::POP(Direct(RAX)));
            }
            Some(els) => {
                let label_else = Label::new("else", id);
                ctx.inss.push(Ins::JE(label_else));
                self.compile_ast(ctx, stmt)?;
                ctx.inss.push(Ins::POP(Direct(RAX)));
                ctx.inss.push(Ins::JMP(label_end));

                ctx.inss.push(Ins::DefLabel(label_else));
                self.compile_ast(ctx, els)?;
                ctx.inss.push(Ins::POP(Direct(RAX)));
            }
        }
        ctx.inss.push(Ins::DefLabel(label_end));
        ctx.inss.push(Ins::PUSH(Direct(RAX)));

        Ok(())
    }

    fn compile_stmt_while(&mut self, ctx: &mut Context, cond: &Ast, stmt: &Ast) -> Result<()> {
        use Opr::*;
        use Reg::*;

        let id = self.increment_label_id();
        let label_begin = Label::new("begin", id);
        let label_end = Label::new("end", id);

        ctx.inss.push(Ins::DefLabel(label_begin));
        self.compile_ast(ctx, cond)?;
        ctx.inss.push(Ins::POP(Direct(RAX)));
        ctx.inss.push(Ins::CMP(Direct(RAX), Literal(0)));
        ctx.inss.push(Ins::JE(label_end));
        self.compile_ast(ctx, stmt)?;
        ctx.inss.push(Ins::POP(Direct(RAX)));
        ctx.inss.push(Ins::JMP(label_begin));
        ctx.inss.push(Ins::DefLabel(label_end));
        ctx.inss.push(Ins::PUSH(Direct(RAX)));

        Ok(())
    }

    fn compile_stmt_for(
        &mut self,
        ctx: &mut Context,
        init: &Option<Box<Ast>>,
        cond: &Option<Box<Ast>>,
        incr: &Option<Box<Ast>>,
        stmt: &Ast,
    ) -> Result<()> {
        use Opr::*;
        use Reg::*;

        let id = self.increment_label_id();
        let label_begin = Label::new("begin", id);
        let label_end = Label::new("end", id);

        if let Some(init) = init {
            self.compile_ast(ctx, init)?;
            ctx.inss.push(Ins::POP(Direct(RAX)));
        }
        ctx.inss.push(Ins::DefLabel(label_begin));
        if let Some(cond) = cond {
            self.compile_ast(ctx, cond)?;
            ctx.inss.push(Ins::POP(Direct(RAX)));
            ctx.inss.push(Ins::CMP(Direct(RAX), Literal(0)));
            ctx.inss.push(Ins::JE(label_end));
        }
        self.compile_ast(ctx, stmt)?;
        if let Some(incr) = incr {
            self.compile_ast(ctx, incr)?;
            ctx.inss.push(Ins::POP(Direct(RAX)));
        }
        ctx.inss.push(Ins::JMP(label_begin));
        ctx.inss.push(Ins::DefLabel(label_end));
        ctx.inss.push(Ins::PUSH(Direct(RAX)));

        Ok(())
    }

    fn compile_num(&mut self, ctx: &mut Context, num: u64) -> Result<()> {
        use Opr::*;

        ctx.inss.push(Ins::PUSH(Literal(num)));

        Ok(())
    }

    fn compile_ident(&mut self, ctx: &mut Context, ast: &Ast) -> Result<()> {
        use Opr::*;
        use Reg::*;

        self.compile_lval(ctx, ast)?;
        ctx.inss.push(Ins::POP(Direct(RAX)));
        ctx.inss.push(Ins::MOV(Direct(RAX), Indirect(RAX)));
        ctx.inss.push(Ins::PUSH(Direct(RAX)));

        Ok(())
    }

    fn compile_binop(&mut self, ctx: &mut Context, binop: &BinOp, l: &Ast, r: &Ast) -> Result<()> {
        use Opr::*;
        use Reg::*;

        if binop.value == BinOpKind::Assign {
            self.compile_lval(ctx, l)?;
            self.compile_ast(ctx, r)?;

            ctx.inss.push(Ins::POP(Direct(R10)));
            ctx.inss.push(Ins::POP(Direct(RAX)));
            ctx.inss.push(Ins::MOV(Indirect(RAX), Direct(R10)));
            ctx.inss.push(Ins::PUSH(Direct(R10)));
            return Ok(());
        }

        self.compile_ast(ctx, l)?;
        self.compile_ast(ctx, r)?;
        ctx.inss.push(Ins::POP(Direct(R10)));
        ctx.inss.push(Ins::POP(Direct(RAX)));
        match binop.value {
            BinOpKind::Assign => unreachable!(),
            BinOpKind::Add => ctx.inss.push(Ins::ADD(Direct(RAX), Direct(R10))),
            BinOpKind::Sub => ctx.inss.push(Ins::SUB(Direct(RAX), Direct(R10))),
            BinOpKind::Mul => ctx.inss.push(Ins::IMUL(Direct(R10))),
            BinOpKind::Div => {
                ctx.inss.push(Ins::CQO);
                ctx.inss.push(Ins::IDIV(Direct(R10)));
            }
            BinOpKind::Eq | BinOpKind::Ne | BinOpKind::Lt | BinOpKind::Le => {
                ctx.inss.push(Ins::CMP(Direct(RAX), Direct(R10)));
                match binop.value {
                    BinOpKind::Eq => ctx.inss.push(Ins::SETE(Direct(AL))),
                    BinOpKind::Ne => ctx.inss.push(Ins::SETNE(Direct(AL))),
                    BinOpKind::Lt => ctx.inss.push(Ins::SETL(Direct(AL))),
                    BinOpKind::Le => ctx.inss.push(Ins::SETLE(Direct(AL))),
                    _ => {}
                }
                ctx.inss.push(Ins::MOVZB(Direct(RAX), Direct(AL)));
            }
            BinOpKind::Gt | BinOpKind::Ge => {
                ctx.inss.push(Ins::CMP(Direct(R10), Direct(RAX)));
                match binop.value {
                    BinOpKind::Gt => ctx.inss.push(Ins::SETL(Direct(AL))),
                    BinOpKind::Ge => ctx.inss.push(Ins::SETLE(Direct(AL))),
                    _ => {}
                }
                ctx.inss.push(Ins::MOVZB(Direct(RAX), Direct(AL)));
            }
        };
        ctx.inss.push(Ins::PUSH(Direct(RAX)));

        Ok(())
    }

    fn compile_uniop(&mut self, ctx: &mut Context, uniop: &UniOp, e: &Ast) -> Result<()> {
        self.compile_ast(ctx, e)?;

        match uniop.value {
            UniOpKind::Positive => {}
            UniOpKind::Negative => {
                // consider -x as 0 - x
                ctx.inss.push(Ins::POP(Opr::Direct(Reg::R10)));
                ctx.inss
                    .push(Ins::MOV(Opr::Direct(Reg::RAX), Opr::Literal(0)));
                ctx.inss
                    .push(Ins::SUB(Opr::Direct(Reg::RAX), Opr::Direct(Reg::R10)));
                ctx.inss.push(Ins::PUSH(Opr::Direct(Reg::RAX)));
            }
        };

        Ok(())
    }

    fn compile_ret(&mut self, ctx: &mut Context, e: &Ast) -> Result<()> {
        use Opr::*;
        use Reg::*;

        self.compile_ast(ctx, e)?;

        ctx.inss.push(Ins::POP(Direct(RAX)));
        ctx.inss.push(Ins::JMP(ctx.ret_label));

        Ok(())
    }

    fn compile_func_call(&mut self, ctx: &mut Context, name: &str, args: &Vec<Ast>) -> Result<()> {
        use Opr::*;
        use Reg::*;

        let org_stackpos = ctx.inss.stackpos;

        for (i, arg) in args.iter().enumerate() {
            self.compile_ast(ctx, arg)?;
            ctx.inss.push(Ins::POP(Direct(Self::ARG_REGS[i])));
        }
        assert!(
            args.len() <= 6,
            "functoin call with more than 6 arguments is not implemented"
        );

        let need_padding = ctx.inss.stackpos % 16 != 0;
        if need_padding {
            ctx.inss.push(Ins::SUB(Direct(RSP), Literal(8)));
            ctx.inss.stackpos += 8;
        }

        ctx.inss.push(Ins::CALL(String::from(name)));

        if need_padding {
            ctx.inss.push(Ins::ADD(Direct(RSP), Literal(8)));
            ctx.inss.stackpos -= 8;
        }
        assert_eq!(ctx.inss.stackpos, org_stackpos);

        // push returned value from called func to stack
        ctx.inss.push(Ins::PUSH(Direct(RAX)));
        Ok(())
    }
}
