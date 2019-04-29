pub mod calling_convention;
pub mod gen;
pub mod instr;
pub mod peephole;
pub mod reg_alloc;

use crate::il::{IlRegisterAllocator, IlRegisterMap};
use crate::sym::SymId;

pub struct Function {
    pub sym: SymId,
    pub instrs: Vec<instr::Instruction>,
    pub regs: IlRegisterAllocator,
    pub reg_map: IlRegisterMap
}
