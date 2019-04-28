#![feature(box_patterns)]
#![feature(drain_filter)]
#![feature(nll)]
#![feature(slice_patterns)]
#![feature(vec_remove_item)]

extern crate itertools;
#[macro_use] extern crate lazy_static;
extern crate plex;
#[macro_use] extern crate smallvec;

macro_rules! matches {
    ($obj:expr, $pat:pat) => {{
        match $obj {
            $pat => true,
            _ => false
        }
    }};
    ($obj:expr, $pat:pat if $cond:expr) => {{
        match $obj {
            $pat if $cond => true,
            _ => false
        }
    }};
}

#[macro_use] pub mod log;

pub mod analysis;
pub mod ast;
pub mod bitvec;
pub mod codegen;
pub mod il;
pub mod ilgen;
pub mod lex;
pub mod optimizer;
pub mod parse;
pub mod sharedio;
pub mod sym;
pub mod types;
pub(crate) mod util;
