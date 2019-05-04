use std::fmt;

use ::ast::{BinOp, UnOp};
use ::lex::Span;
use ::types::{PrettyType, TypeTable, TypeId};

#[derive(Debug, Clone)]
pub enum ErrorKind {
    CannotConvert { expected: TypeId, actual: TypeId },
    UndeclaredIdentifier(String),
    UndeclaredType(String),
    CannotCallType(TypeId),
    InvalidCallSignature { func_type: TypeId, arg_types: Vec<TypeId> },
    NoSuchBinOp { op: BinOp, lhs_type: TypeId, rhs_type: TypeId },
    NoSuchUnOp { op: UnOp, val_type: TypeId },
    CannotAssignExpr,
    CannotAssignImmutable(String),
    LambdaCaptureNotSupported,
    CannotInferType
}

#[derive(Debug, Clone)]
pub struct Error(pub ErrorKind, pub Span);

pub struct PrettyError<'a>(pub &'a Error, pub &'a TypeTable);

impl <'a> fmt::Display for PrettyError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let PrettyError(error, type_table) = self;

        write!(f, "line {}, col {}: ", error.1.lo.line, error.1.lo.col)?;

        match error.0 {
            ErrorKind::CannotConvert { expected, actual } => {
                write!(f, "cannot convert from {} to {}",
                    PrettyType(actual, type_table),
                    PrettyType(expected, type_table)
                )?;
            },
            ErrorKind::UndeclaredIdentifier(ref name) => {
                write!(f, "no identifier {} declared in this scope", name)?;
            },
            ErrorKind::UndeclaredType(ref name) => {
                write!(f, "no type {} declared in this scope", name)?;
            },
            ErrorKind::CannotCallType(ty) => {
                write!(f, "cannot call a value of type {}", PrettyType(ty, type_table))?;
            },
            ErrorKind::InvalidCallSignature { func_type, ref arg_types } => {
                write!(f, "cannot call a value of type {} with arguments (", PrettyType(func_type, type_table))?;
                match &arg_types[..] {
                    [ head, tail.. ] => {
                        write!(f, "{}", PrettyType(*head, type_table))?;
                        for t in tail {
                            write!(f, ", {}", PrettyType(*t, type_table))?;
                        };
                    },
                    [] => {}
                };
                write!(f, ")")?;
            },
            ErrorKind::NoSuchBinOp { op, lhs_type, rhs_type } => {
                write!(f, "no operator {} exists for {} and {}",
                    op,
                    PrettyType(lhs_type, type_table),
                    PrettyType(rhs_type, type_table)
                )?;
            },
            ErrorKind::NoSuchUnOp { op, val_type } => {
                write!(f, "no operator {} exists for {}",
                    op,
                    PrettyType(val_type, type_table)
                )?;
            },
            ErrorKind::CannotAssignExpr => {
                write!(f, "cannot assign this expression")?;
            },
            ErrorKind::CannotAssignImmutable(ref name) => {
                write!(f, "cannot assign to immutable variable {}", name)?;
            },
            ErrorKind::LambdaCaptureNotSupported => {
                write!(f, "lambda capture is not yet supported")?;
            },
            ErrorKind::CannotInferType => {
                write!(f, "unable to infer the type of this expression")?;
            }
        }

        Result::Ok(())
    }
}
