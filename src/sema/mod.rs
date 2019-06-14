use std::fmt;

use crate::lexer::{Annot, Loc};
use crate::parser::{Ast, AstNode, BinOpKind, Type, UniOp, UniOpKind};

#[cfg(test)]
#[cfg_attr(tarpaulin, skip)]
mod tests;

pub type Result<T> = std::result::Result<T, SemanticError>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SemanticErrorKind {
    LValRequired,
    NotAnInteger,
}

#[derive(Fail, Debug, Clone, PartialEq, Eq, Hash)]
pub struct SemanticError(Annot<SemanticErrorKind>);

impl SemanticError {
    fn new(kind: SemanticErrorKind, loc: Loc) -> Self {
        SemanticError(Annot::new(kind, loc))
    }

    fn lval_required(loc: Loc) -> Self {
        SemanticError::new(SemanticErrorKind::LValRequired, loc)
    }
    fn not_an_integer(loc: Loc) -> Self {
        SemanticError::new(SemanticErrorKind::NotAnInteger, loc)
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

impl fmt::Display for SemanticError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::SemanticErrorKind::*;
        match self.0.value {
            LValRequired => write!(
                f,
                "{}: lvalue required as left operand of assignment",
                self.0.loc
            ),
            NotAnInteger => write!(f, "{}: not an integer", self.0.loc),
        }
    }
}

pub fn analyze(ast: &mut Ast) -> Result<()> {
    let funcs = match &mut ast.value {
        AstNode::Program { funcs } => funcs,
        _ => unreachable!("root ast node should be Program"),
    };
    for func in funcs {
        let (_name, _args, _lvars, mut body) = match &mut func.value {
            AstNode::Func {
                name,
                args,
                lvars,
                body,
            } => (name, args, lvars, body),
            _ => unreachable!("ast node under Program should be Func"),
        };
        walk(&mut body)?;
    }

    Ok(())
}

fn walk(ast: &mut Ast) -> Result<()> {
    use AstNode::*;

    // TODO: check types
    match ast.value {
        Program { .. } => unreachable!("invalid ast structure"),
        Func { .. } => unreachable!("invalid ast structure"),
        Block(ref mut stmts) => {
            for stmt in stmts {
                walk(stmt)?;
            }
        }
        StmtIf {
            ref mut cond,
            ref mut stmt,
            ref mut els,
        } => {
            walk(cond)?;
            walk(stmt)?;
            if let Some(els) = els {
                walk(els)?;
            }
        }
        StmtWhile {
            ref mut cond,
            ref mut stmt,
        } => {
            walk(cond)?;
            walk(stmt)?;
        }
        StmtFor {
            ref mut init,
            ref mut cond,
            ref mut incr,
            ref mut stmt,
        } => {
            if let Some(init) = init {
                walk(init)?;
            }
            if let Some(cond) = cond {
                walk(cond)?;
            }
            if let Some(incr) = incr {
                walk(incr)?;
            }
            walk(stmt)?;
        }
        StmtNull => {}
        Num(_) => {}
        VarRef { .. } => {}
        BinOp {
            ref op,
            ref mut l,
            ref mut r,
        } => match op.value {
            BinOpKind::Assign => {
                walk(l)?;
                check_lval(l)?;
                walk(r)?;
                ast.ty = l.ty.clone();
            }
            BinOpKind::Add => {
                walk(l)?;
                walk(r)?;

                if let Type::Ptr(ty) = l.get_type() {
                    check_int(r)?;
                    scale_pointer(true, r, &ty);
                    ast.ty = l.ty.clone();
                } else if let Type::Ptr(ty) = r.get_type() {
                    check_int(l)?;
                    scale_pointer(true, l, &ty);
                    ast.ty = r.ty.clone();
                } else {
                    check_int(l)?;
                    check_int(r)?;
                    ast.ty = Some(Type::Int);
                }
            }
            BinOpKind::Sub => {
                walk(l)?;
                walk(r)?;

                // TODO: support pointers
                check_int(l)?;
                check_int(r)?;
                ast.ty = Some(Type::Int);
            }
            _ => {
                walk(l)?;
                walk(r)?;

                // TODO: support pointers
                check_int(l)?;
                check_int(r)?;
                ast.ty = Some(Type::Int);
            }
        },
        UniOp { ref op, ref mut e } => match op.value {
            // TODO: support addr and deref
            _ => {
                walk(e)?;
                check_int(e)?;
                ast.ty = Some(Type::Int);
            }
        },
        Ret { ref mut e } => walk(e)?,
        FuncCall { ref mut args, .. } => {
            for arg in args {
                walk(arg)?;
            }
            // TODO: support returning type
            ast.ty = Some(Type::Int);
        }
    }

    Ok(())
}

fn check_lval(ast: &Ast) -> Result<()> {
    match ast.value {
        AstNode::VarRef { .. }
        | AstNode::UniOp {
            op: UniOp {
                value: UniOpKind::Deref,
                ..
            },
            ..
        } => Ok(()),
        _ => Err(SemanticError::lval_required(ast.loc.clone())),
    }
}

fn check_int(ast: &Ast) -> Result<()> {
    match ast.ty {
        Some(Type::Int) => Ok(()),
        _ => Err(SemanticError::not_an_integer(ast.loc.clone())),
    }
}

fn scale_pointer(is_mul: bool, ast: &mut Ast, child_ty: &Type) {
    match ast.value {
        AstNode::Num(ref mut n) => {
            if is_mul {
                *n *= child_ty.size();
            } else {
                unimplemented!("pointer subtraction is not implemented yet");
            }
        }
        _ => unreachable!("could not scale pointer"),
    }
}
