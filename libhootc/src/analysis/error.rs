use std::fmt;

use ::ast::{BinOp, UnOp};
use ::lex::Span;
use ::types::{PrettyType, TypeTable, TypeId};

#[macro_export]
macro_rules! expect_type {
    ($expr:expr, $etype:expr, $ctx:ident) => {{
        if $expr.ty != $etype && !$expr.ty.is_error() {
            cannot_convert!($expr.span, $etype, $expr.ty, $ctx);
            false
        } else {
            true
        }
    }}
}

#[macro_export]
macro_rules! expect_resolved_type {
    ($expr:expr, $ctx:ident) => {{
        if $ctx.types.is_type_undecided($expr.ty) {
            cannot_infer_type!($expr, $ctx);
            false
        } else {
            true
        }
    }}
}

#[macro_export]
macro_rules! cannot_convert {
    ($span:expr, $etype:expr, $atype:expr, $ctx:ident) => {{
        $ctx.errors.push(Error(
            ErrorKind::CannotConvert { expected: $etype, actual: $atype },
            $span
        ));
    }}
}

#[macro_export]
macro_rules! cannot_call {
    ($expr:expr, $func:expr, $ctx:ident) => {{
        $ctx.errors.push(Error(
            ErrorKind::CannotCallType($func.ty),
            $expr.span
        ))
    }}
}

#[macro_export]
macro_rules! invalid_call_signature {
    ($expr:expr, $func:expr, $arg_types:expr, $ctx:ident) => {{
        $ctx.errors.push(Error(
            ErrorKind::InvalidCallSignature { func_type: $func.ty, arg_types: $arg_types },
            $expr.span
        ))
    }}
}

#[macro_export]
macro_rules! invalid_bin_op_type {
    ($expr:expr, $op:expr, $lhs:expr, $rhs:expr, $ctx:ident) => {{
        $ctx.errors.push(Error(
            ErrorKind::NoSuchBinOp { op: $op, lhs_type: $lhs.ty, rhs_type: $rhs.ty },
            $expr.span
        ))
    }}
}

#[macro_export]
macro_rules! invalid_un_op_type {
    ($expr:expr, $op:expr, $val:expr, $ctx:ident) => {{
        $ctx.errors.push(Error(
            ErrorKind::NoSuchUnOp { op: $op, val_type: $val.ty },
            $expr.span
        ))
    }}
}

#[macro_export]
macro_rules! cannot_assign {
    ($expr:expr, $ctx:ident) => {{
        $ctx.errors.push(Error(
            ErrorKind::CannotAssignExpr,
            $expr.span
        ))
    }}
}

#[macro_export]
macro_rules! cannot_assign_immutable {
    ($expr:expr, $id:expr, $ctx:ident) => {{
        $ctx.errors.push(Error(
            ErrorKind::CannotAssignImmutable($id),
            $expr.span
        ))
    }}
}

#[macro_export]
macro_rules! lambda_capture_not_supported {
    ($expr:expr, $ctx:ident) => {{
        $ctx.errors.push(Error(
            ErrorKind::LambdaCaptureNotSupported,
            $expr.span
        ))
    }}
}

#[macro_export]
macro_rules! cannot_infer_type {
    ($expr:expr, $ctx:ident) => {{
        $ctx.errors.push(Error(
            ErrorKind::CannotInferType,
            $expr.span
        ))
    }}
}

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
                if !arg_types.is_empty() {
                    write!(f, "{}", PrettyType(arg_types[0], type_table))?;
                    for t in &arg_types[1..] {
                        write!(f, ", {}", PrettyType(*t, type_table))?;
                    };
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
