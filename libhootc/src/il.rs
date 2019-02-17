use std::collections::HashMap;
use std::fmt;

use super::lex::Span;
use super::sym::SymId;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IlBlockId(pub u32);

impl IlBlockId {
    pub fn dummy() -> IlBlockId {
        IlBlockId(!0)
    }
}

impl fmt::Display for IlBlockId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.0 == !0 {
            write!(f, "L??")
        } else {
            write!(f, "L{}", self.0)
        }
    }
}

impl From<IlBlockId> for usize {
    fn from(id: IlBlockId) -> usize {
        id.0 as usize
    }
}

impl From<usize> for IlBlockId {
    fn from(id: usize) -> IlBlockId {
        IlBlockId(id as u32)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IlRegister(pub u32);

impl fmt::Display for IlRegister {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "${}", self.0)
    }
}

impl From<IlRegister> for usize {
    fn from(r: IlRegister) -> usize {
        r.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IlType {
    I32,
    Addr,
    Void
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IlConst {
    I32(i32)
}

impl fmt::Display for IlConst {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            IlConst::I32(val) => write!(f, "i32:{}", val)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IlOperand {
    Register(IlRegister),
    Const(IlConst)
}

impl fmt::Display for IlOperand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            IlOperand::Register(ref r) => write!(f, "{}", r),
            IlOperand::Const(ref c) => write!(f, "{}", c)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IlInstructionKind {
    Nop,
    Copy(IlRegister, IlOperand),
    NegI32(IlRegister, IlOperand),
    NotI32(IlRegister, IlOperand),
    LogicNotI32(IlRegister, IlOperand),
    AddI32(IlRegister, IlOperand, IlOperand),
    SubI32(IlRegister, IlOperand, IlOperand),
    MulI32(IlRegister, IlOperand, IlOperand),
    EqI32(IlRegister, IlOperand, IlOperand),
    NeI32(IlRegister, IlOperand, IlOperand),
    PrintI32(IlOperand),
    Call(IlRegister, Vec<IlOperand>, SymId)
}

impl fmt::Display for IlInstructionKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::IlInstructionKind::*;
        match *self {
            Nop => write!(f, "nop"),
            Copy(ref tgt, ref o) => write!(f, "copy {} {}", tgt, o),
            NegI32(ref tgt, ref o) => write!(f, "neg.i32 {} {}", tgt, o),
            NotI32(ref tgt, ref o) => write!(f, "not.i32 {} {}", tgt, o),
            LogicNotI32(ref tgt, ref o) => write!(f, "lnot.i32 {} {}", tgt, o),
            AddI32(ref tgt, ref o1, ref o2) => write!(f, "add.i32 {} {} {}", tgt, o1, o2),
            SubI32(ref tgt, ref o1, ref o2) => write!(f, "sub.i32 {} {} {}", tgt, o1, o2),
            MulI32(ref tgt, ref o1, ref o2) => write!(f, "mul.i32 {} {} {}", tgt, o1, o2),
            EqI32(ref tgt, ref o1, ref o2) => write!(f, "eq.i32 {} {} {}", tgt, o1, o2),
            NeI32(ref tgt, ref o1, ref o2) => write!(f, "ne.i32 {} {} {}", tgt, o1, o2),
            PrintI32(ref o) => write!(f, "print.i32 {}", o),
            Call(ref tgt, ref os, ref func) => {
                write!(f, "call {}", tgt)?;
                for o in os {
                    write!(f, " {}", o)?;
                };
                write!(f, " {}", func)?;
                Result::Ok(())
            }
        }
    }
}

impl IlInstructionKind {
    pub fn target(&self) -> Option<IlRegister> {
        use self::IlInstructionKind::*;
        match *self {
            Nop => None,
            Copy(tgt, _) => Some(tgt),
            NegI32(tgt, _) => Some(tgt),
            NotI32(tgt, _) => Some(tgt),
            LogicNotI32(tgt, _) => Some(tgt),
            AddI32(tgt, _, _) => Some(tgt),
            SubI32(tgt, _, _) => Some(tgt),
            MulI32(tgt, _, _) => Some(tgt),
            EqI32(tgt, _, _) => Some(tgt),
            NeI32(tgt, _, _) => Some(tgt),
            PrintI32(_) => None,
            Call(tgt, _, _) => Some(tgt)
        }
    }

    pub fn target_mut(&mut self) -> Option<&mut IlRegister> {
        use self::IlInstructionKind::*;
        match *self {
            Nop => None,
            Copy(ref mut tgt, _) => Some(tgt),
            NegI32(ref mut tgt, _) => Some(tgt),
            NotI32(ref mut tgt, _) => Some(tgt),
            LogicNotI32(ref mut tgt, _) => Some(tgt),
            AddI32(ref mut tgt, _, _) => Some(tgt),
            SubI32(ref mut tgt, _, _) => Some(tgt),
            MulI32(ref mut tgt, _, _) => Some(tgt),
            EqI32(ref mut tgt, _, _) => Some(tgt),
            NeI32(ref mut tgt, _, _) => Some(tgt),
            PrintI32(_) => None,
            Call(ref mut tgt, _, _) => Some(tgt)
        }
    }

    pub fn for_operands<F: FnMut (&IlOperand) -> ()>(&self, mut f: F) {
        use self::IlInstructionKind::*;
        match *self {
            Nop => {},
            Copy(_, ref o) => {
                f(o);
            },
            NegI32(_, ref o) => {
                f(o);
            },
            NotI32(_, ref o) => {
                f(o);
            },
            LogicNotI32(_, ref o) => {
                f(o);
            },
            AddI32(_, ref o1, ref o2) => {
                f(o1);
                f(o2);
            },
            SubI32(_, ref o1, ref o2) => {
                f(o1);
                f(o2);
            },
            MulI32(_, ref o1, ref o2) => {
                f(o1);
                f(o2);
            },
            EqI32(_, ref o1, ref o2) => {
                f(o1);
                f(o2);
            },
            NeI32(_, ref o1, ref o2) => {
                f(o1);
                f(o2);
            },
            PrintI32(ref o) => {
                f(o);
            },
            Call(_, ref os, _) => {
                os.iter().for_each(f);
            }
        };
    }

    pub fn for_operands_mut<F: FnMut (&mut IlOperand) -> ()>(&mut self, mut f: F) {
        use self::IlInstructionKind::*;
        match *self {
            Nop => {},
            Copy(_, ref mut o) => {
                f(o);
            },
            NegI32(_, ref mut o) => {
                f(o);
            },
            NotI32(_, ref mut o) => {
                f(o);
            },
            LogicNotI32(_, ref mut o) => {
                f(o);
            }
            AddI32(_, ref mut o1, ref mut o2) => {
                f(o1);
                f(o2);
            },
            SubI32(_, ref mut o1, ref mut o2) => {
                f(o1);
                f(o2);
            },
            MulI32(_, ref mut o1, ref mut o2) => {
                f(o1);
                f(o2);
            },
            EqI32(_, ref mut o1, ref mut o2) => {
                f(o1);
                f(o2);
            },
            NeI32(_, ref mut o1, ref mut o2) => {
                f(o1);
                f(o2);
            },
            PrintI32(ref mut o) => {
                f(o);
            },
            Call(_, ref mut os, _) => {
                os.iter_mut().for_each(f);
            }
        };
    }

    pub fn can_dead_store_eliminate(&self) -> bool {
        use self::IlInstructionKind::*;
        match *self {
            PrintI32(_) => false,
            Call(_, _, _) => false,
            _ => true
        }
    }

    pub fn can_move_from_loop(&self) -> bool {
        use self::IlInstructionKind::*;
        match *self {
            Copy(_, _) => false,
            PrintI32(_) => false,
            Call(_, _, _) => false,
            _ => true
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IlEndingInstructionKind {
    Nop,
    Jump(IlBlockId),
    JumpNonZero(IlBlockId, IlOperand),
    JumpZero(IlBlockId, IlOperand),
    Return(IlOperand)
}

impl IlEndingInstructionKind {
    pub fn for_operands<F: FnMut (&IlOperand) -> ()>(&self, mut f: F) {
        use self::IlEndingInstructionKind::*;
        match *self {
            Nop => {},
            Jump(_) => {},
            JumpNonZero(_, ref cond) => f(cond),
            JumpZero(_, ref cond) => f(cond),
            Return(ref val) => f(val),
        };
    }

    pub fn for_operands_mut<F: FnMut (&mut IlOperand) -> ()>(&mut self, mut f: F) {
        use self::IlEndingInstructionKind::*;
        match *self {
            Nop => {},
            Jump(_) => {},
            JumpNonZero(_, ref mut cond) => f(cond),
            JumpZero(_, ref mut cond) => f(cond),
            Return(ref mut val) => f(val)
        };
    }

    pub fn can_fall_through(&self) -> bool {
        use self::IlEndingInstructionKind::*;
        match *self {
            Jump(_) => false,
            Return(_) => false,
            _ => true
        }
    }

    pub fn target_block(&self) -> Option<IlBlockId> {
        use self::IlEndingInstructionKind::*;
        match *self {
            Nop => None,
            Jump(target) => Some(target),
            JumpNonZero(target, _) => Some(target),
            JumpZero(target, _) => Some(target),
            Return(_) => None
        }
    }

    pub fn set_target_block(&mut self, id: IlBlockId) {
        use self::IlEndingInstructionKind::*;
        match *self {
            Jump(ref mut target) => { *target = id; },
            JumpNonZero(ref mut target, _) => { *target = id; },
            JumpZero(ref mut target, _) => { *target = id; },
            _ => unreachable!()
        };
    }

    pub fn is_unconditional_jump_to(&self, id: IlBlockId) -> bool {
        use self::IlEndingInstructionKind::*;
        match *self {
            Jump(target) => target == id,
            _ => false
        }
    }
}

impl fmt::Display for IlEndingInstructionKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::IlEndingInstructionKind::*;
        match *self {
            Nop => write!(f, "nop"),
            Jump(IlBlockId(target)) => write!(f, "j L{}", target),
            JumpNonZero(IlBlockId(target), ref cond) => write!(f, "jnz L{}, {}", target, cond),
            JumpZero(IlBlockId(target), ref cond) => write!(f, "jz L{}, {}", target, cond),
            Return(ref val) => write!(f, "ret {}", val),
        }
    }
}

#[derive(Clone)]
pub struct IlWrappedInstruction<T> {
    pub node: T,
    pub span: Span
}

impl <T> IlWrappedInstruction<T> {
    pub fn new(node: T, span: Span) -> IlWrappedInstruction<T> {
        IlWrappedInstruction { node, span }
    }
}

impl <T: fmt::Display> fmt::Display for IlWrappedInstruction<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.node)?;

        if self.span != Span::dummy() {
            let Span { lo, hi } = self.span;
            write!(f, " [span: {}:{} - {}:{}]", lo.line, lo.col, hi.line, hi.col)?;
        };

        Result::Ok(())
    }
}

pub type IlInstruction = IlWrappedInstruction<IlInstructionKind>;
pub type IlEndingInstruction = IlWrappedInstruction<IlEndingInstructionKind>;

pub struct IlBlock {
    pub id: IlBlockId,
    pub instrs: Vec<IlInstruction>,
    pub end_instr: IlEndingInstruction
}

impl IlBlock {
    pub fn new() -> IlBlock {
        IlBlock {
            id: IlBlockId(!0),
            instrs: vec![],
            end_instr: IlEndingInstruction::new(IlEndingInstructionKind::Nop, Span::dummy())
        }
    }
}

impl Default for IlBlock {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for IlBlock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "L{}:", self.id.0)?;

        for i in self.instrs.iter() {
            write!(f, "\n  {}", i)?;
        };

        if !matches!(self.end_instr.node, IlEndingInstructionKind::Nop) {
            write!(f, "\n  {}", self.end_instr)?;
        };

        Result::Ok(())
    }
}

pub struct IlRegisterAllocator {
    next: IlRegister
}

impl IlRegisterAllocator {
    pub fn new() -> IlRegisterAllocator {
        IlRegisterAllocator { next: IlRegister(0) }
    }

    pub fn allocate(&mut self) -> IlRegister {
        let r = self.next;
        self.next = IlRegister(r.0 + 1);
        r
    }
}

impl Default for IlRegisterAllocator {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub enum IlRegisterType {
    Local(SymId),
    Param(SymId, u32),
    Temp
}

#[derive(Debug, Clone)]
pub struct IlRegisterInfo(pub IlRegisterType, pub IlType);

#[derive(Debug, Clone)]
pub struct IlRegisterMap {
    reg_info: HashMap<IlRegister, IlRegisterInfo>,
    sym_regs: HashMap<SymId, IlRegister>,
    param_regs: Vec<IlRegister>
}

impl IlRegisterMap {
    pub fn new() -> IlRegisterMap {
        IlRegisterMap {
            reg_info: HashMap::new(),
            sym_regs: HashMap::new(),
            param_regs: vec![]
        }
    }

    pub fn get_sym_reg(&self, id: SymId) -> Option<IlRegister> {
        self.sym_regs.get(&id).cloned()
    }

    pub fn add_sym_reg(&mut self, id: SymId, reg: IlRegister) {
        self.sym_regs.insert(id, reg);
    }

    pub fn get_reg_info(&self, reg: IlRegister) -> &IlRegisterInfo {
        &self.reg_info[&reg]
    }

    pub fn add_reg_info(&mut self, reg: IlRegister, info: IlRegisterInfo) {
        self.reg_info.insert(reg, info);
    }

    pub fn set_params(&mut self, params: Vec<IlRegister>) {
        self.param_regs = params;
    }

    pub fn params(&self) -> &[IlRegister] {
        &self.param_regs
    }
}

impl Default for IlRegisterMap {
    fn default() -> Self {
        Self::new()
    }
}

pub struct IlFunction {
    pub blocks: HashMap<IlBlockId, IlBlock>,
    pub block_order: Vec<IlBlockId>,
    pub reg_alloc: IlRegisterAllocator,
    pub reg_map: IlRegisterMap,
    pub next_block_id: IlBlockId
}

impl IlFunction {
    pub fn new() -> IlFunction {
        IlFunction {
            blocks: HashMap::new(),
            block_order: vec![],
            reg_alloc: IlRegisterAllocator::new(),
            reg_map: IlRegisterMap::new(),
            next_block_id: IlBlockId(0)
        }
    }

    pub fn add_block(&mut self, mut block: IlBlock) -> IlBlockId {
        let id = self.next_block_id;
        self.next_block_id = IlBlockId(self.next_block_id.0 + 1);

        block.id = id;
        self.blocks.insert(id, block);
        id
    }

    pub fn append_block(&mut self, block: IlBlock) -> IlBlockId {
        let id = self.add_block(block);

        self.block_order.push(id);
        id
    }
}

impl Default for IlFunction {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for IlFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (&reg, reg_info) in self.reg_map.reg_info.iter() {
            match reg_info.0 {
                IlRegisterType::Local(id) => {
                    writeln!(f, ".local {} {}", reg, id)?;
                },
                IlRegisterType::Param(id, n) => {
                    writeln!(f, ".local {} {}", reg, id)?;
                    writeln!(f, ".param {} {}", reg, n)?;
                },
                _ => {}
            };
        };

        for block_id in self.block_order.iter() {
            writeln!(f, "{}", self.blocks[block_id])?;
        };
        Result::Ok(())
    }
}

pub struct IlProgram {
    pub funcs: HashMap<SymId, IlFunction>
}

impl IlProgram {
    pub fn new() -> IlProgram {
        IlProgram { funcs: HashMap::new() }
    }
}

impl Default for IlProgram {
    fn default() -> Self {
        Self::new()
    }
}
