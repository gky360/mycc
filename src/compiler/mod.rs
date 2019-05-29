use crate::asm::{Assembly, Ent, Function, Ins, Opr, Reg};
use crate::parser::{Ast, AstNode, BinOp, BinOpKind, UniOp, UniOpKind};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Compiler;

impl Compiler {
    pub fn new() -> Compiler {
        Compiler
    }

    pub fn compile(&mut self, expr: &Ast) -> Assembly {
        let mut inss = Vec::new();
        self.compile_expr(expr, &mut inss);
        inss.push(Ins::POP(Opr::Direct(Reg::RAX)));
        inss.push(Ins::RET);

        let fn_main = Function::new("main", inss);
        Assembly::new(vec![
            Ent::dot("intel_syntax", "noprefix"),
            Ent::dot("global", "main"),
            Ent::Fun(fn_main),
        ])
    }

    fn compile_expr(&mut self, expr: &Ast, inss: &mut Vec<Ins>) {
        match expr.value {
            AstNode::Num(num) => {
                self.compile_num(num, inss);
            }
            AstNode::BinOp {
                ref op,
                ref l,
                ref r,
            } => {
                self.compile_expr(l, inss);
                self.compile_expr(r, inss);
                self.compile_binop(op, inss);
            }
            AstNode::UniOp { ref op, ref e } => {
                self.compile_expr(e, inss);
                self.compile_uniop(op, inss);
            }
        }
    }

    fn compile_num(&mut self, num: u64, inss: &mut Vec<Ins>) {
        inss.push(Ins::PUSH(Opr::Literal(num)));
    }

    fn compile_binop(&mut self, binop: &BinOp, inss: &mut Vec<Ins>) {
        inss.push(Ins::POP(Opr::Direct(Reg::RDI)));
        inss.push(Ins::POP(Opr::Direct(Reg::RAX)));
        match binop.value {
            BinOpKind::Add => inss.push(Ins::ADD(Opr::Direct(Reg::RAX), Opr::Direct(Reg::RDI))),
            BinOpKind::Sub => inss.push(Ins::SUB(Opr::Direct(Reg::RAX), Opr::Direct(Reg::RDI))),
            BinOpKind::Mul => inss.push(Ins::IMUL(Opr::Direct(Reg::RDI))),
            BinOpKind::Div => {
                inss.push(Ins::CQO);
                inss.push(Ins::IDIV(Opr::Direct(Reg::RDI)));
            }
        };
        inss.push(Ins::PUSH(Opr::Direct(Reg::RAX)));
    }

    fn compile_uniop(&mut self, uniop: &UniOp, inss: &mut Vec<Ins>) {
        match uniop.value {
            UniOpKind::Positive => {}
            UniOpKind::Negative => {
                inss.push(Ins::POP(Opr::Direct(Reg::RDI)));
                inss.push(Ins::MOV(Opr::Direct(Reg::RAX), Opr::Literal(0)));
                inss.push(Ins::SUB(Opr::Direct(Reg::RAX), Opr::Direct(Reg::RDI)));
                inss.push(Ins::PUSH(Opr::Direct(Reg::RAX)));
            }
        };
    }
}