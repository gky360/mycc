use std::fmt;

use crate::lexer::{Annot, Loc};
use crate::parser::{Ast, AstNode, BinOp, BinOpKind, Type, UniOp, UniOpKind};

#[cfg(test)]
#[cfg_attr(tarpaulin, skip)]
mod tests;

pub type Result<T> = std::result::Result<T, SemanticError>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SemanticErrorKind {
    LValRequired,
    UnexpectedType(Type),
}

#[derive(Fail, Debug, Clone, PartialEq, Eq, Hash)]
pub struct SemanticError(Annot<SemanticErrorKind>);

impl SemanticError {
    fn new(kind: SemanticErrorKind, loc: Loc) -> Self {
        SemanticError(Annot::new(kind, loc))
    }

    fn lval_required(loc: &Loc) -> Self {
        SemanticError::new(SemanticErrorKind::LValRequired, loc.clone())
    }
    fn unexpected_type(ty: &Type, loc: &Loc) -> Self {
        SemanticError::new(SemanticErrorKind::UnexpectedType(ty.clone()), loc.clone())
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
        match &self.0.value {
            LValRequired => write!(
                f,
                "{}: lvalue required as left operand of assignment",
                self.0.loc
            ),
            UnexpectedType(ty) => write!(f, "{}: unexpected type: {}", self.0.loc, ty),
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
    do_walk(ast, true)
}

fn walk_nodecay(ast: &mut Ast) -> Result<()> {
    do_walk(ast, false)
}

fn do_walk(ast: &mut Ast, should_decay: bool) -> Result<()> {
    use AstNode::*;

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
        Num(_) => ast.ty = Some(Type::Int),
        VarRef { ref ty, .. } => {
            ast.ty = Some(ty.clone());
            maybe_decay(ast, should_decay);
        }
        BinOp {
            ref op,
            ref mut l,
            ref mut r,
        } => match op.value {
            BinOpKind::Assign => {
                walk_nodecay(l)?;
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
            UniOpKind::Addr => {
                walk(e)?;
                check_lval(e)?;
                ast.ty = Some(Type::ptr(e.get_type().clone()));
            }
            UniOpKind::Deref => {
                walk(e)?;
                match e.get_type() {
                    Type::Ptr(ty) => {
                        ast.ty = Some(ty.as_ref().clone());
                        maybe_decay(ast, should_decay);
                    }
                    _ => return Err(SemanticError::unexpected_type(e.get_type(), &e.loc)),
                };
            }
            UniOpKind::Sizeof => {
                walk_nodecay(e)?;
                ast.value = AstNode::Num(e.get_type().size());
                ast.ty = Some(Type::Int);
            }
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

fn maybe_decay(ast: &mut Ast, should_decay: bool) {
    if !should_decay {
        return;
    }
    match &ast.ty {
        Some(Type::Array(ty, _len)) => {
            ast.value = AstNode::UniOp {
                op: UniOp::new(UniOpKind::Addr, Loc::NONE),
                e: Box::new(ast.clone()),
            };
            ast.ty = Some(Type::ptr(ty.as_ref().clone()));
        }
        _ => {}
    }
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
        _ => Err(SemanticError::lval_required(&ast.loc)),
    }
}

fn check_int(ast: &Ast) -> Result<()> {
    match ast.get_type() {
        Type::Int => Ok(()),
        ty => Err(SemanticError::unexpected_type(ty, &ast.loc)),
    }
}

fn scale_pointer(is_mul: bool, ast: &mut Ast, child_ty: &Type) {
    let op = if is_mul {
        BinOp::mul(Loc::NONE)
    } else {
        unimplemented!("pointer subtraction is not implemented yet")
    };
    ast.value = AstNode::BinOp {
        op,
        l: Box::new(ast.clone()),
        r: Box::new(Ast::num(child_ty.size(), Loc::NONE)),
    };
}
