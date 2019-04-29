use std::fmt;

use crate::codegen::label::Label;
use crate::il::{IlRegister, IlSpanId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Condition {
    Above,
    AboveOrEqual,
    Below,
    BelowOrEqual,
    Greater,
    GreaterOrEqual,
    Less,
    LessOrEqual,
    Equal,
    NotEqual,
    Overflow,
    NotOverflow,
    ParityEven,
    ParityOdd,
    Sign,
    NotSign
}

impl Condition {
    pub fn name(&self) -> &'static str {
        match *self {
            Condition::Above => "a",
            Condition::AboveOrEqual => "ae",
            Condition::Below => "b",
            Condition::BelowOrEqual => "be",
            Condition::Greater => "g",
            Condition::GreaterOrEqual => "ge",
            Condition::Less => "l",
            Condition::LessOrEqual => "le",
            Condition::Equal => "e",
            Condition::NotEqual => "ne",
            Condition::Overflow => "o",
            Condition::NotOverflow => "no",
            Condition::ParityEven => "pe",
            Condition::ParityOdd => "po",
            Condition::Sign => "s",
            Condition::NotSign => "ns"
        }
    }

    pub fn reverse(self) -> Condition {
        match self {
            Condition::Above => Condition::BelowOrEqual,
            Condition::AboveOrEqual => Condition::Below,
            Condition::Below => Condition::AboveOrEqual,
            Condition::BelowOrEqual => Condition::Above,
            Condition::Greater => Condition::LessOrEqual,
            Condition::GreaterOrEqual => Condition::Less,
            Condition::Less => Condition::GreaterOrEqual,
            Condition::LessOrEqual => Condition::Greater,
            Condition::Equal => Condition::NotEqual,
            Condition::NotEqual => Condition::Equal,
            Condition::Overflow => Condition::NotOverflow,
            Condition::NotOverflow => Condition::Overflow,
            Condition::ParityEven => Condition::ParityOdd,
            Condition::ParityOdd => Condition::ParityEven,
            Condition::Sign => Condition::NotSign,
            Condition::NotSign => Condition::Sign
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RegisterSize {
    Byte,
    Word,
    DWord,
    QWord
}

impl RegisterSize {
    pub fn for_size(size: i32) -> Option<RegisterSize> {
        match size {
            1 => Some(RegisterSize::Byte),
            2 => Some(RegisterSize::Word),
            4 => Some(RegisterSize::DWord),
            8 => Some(RegisterSize::QWord),
            _ => None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RealRegister {
    None,
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rbp,
    Rsp,
    Rsi,
    Rdi,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15
}

impl RealRegister {
    pub fn name_qword(&self) -> &'static str {
        match *self {
            RealRegister::None => "???",
            RealRegister::Rax => "rax",
            RealRegister::Rbx => "rbx",
            RealRegister::Rcx => "rcx",
            RealRegister::Rdx => "rdx",
            RealRegister::Rbp => "rbp",
            RealRegister::Rsp => "rsp",
            RealRegister::Rsi => "rsi",
            RealRegister::Rdi => "rdi",
            RealRegister::R8 => "r8",
            RealRegister::R9 => "r9",
            RealRegister::R10 => "r10",
            RealRegister::R11 => "r11",
            RealRegister::R12 => "r12",
            RealRegister::R13 => "r13",
            RealRegister::R14 => "r14",
            RealRegister::R15 => "r15"
        }
    }

    pub fn name_dword(&self) -> &'static str {
        match *self {
            RealRegister::None => "???",
            RealRegister::Rax => "eax",
            RealRegister::Rbx => "ebx",
            RealRegister::Rcx => "ecx",
            RealRegister::Rdx => "edx",
            RealRegister::Rbp => "ebp",
            RealRegister::Rsp => "esp",
            RealRegister::Rsi => "esi",
            RealRegister::Rdi => "edi",
            RealRegister::R8 => "r8d",
            RealRegister::R9 => "r9d",
            RealRegister::R10 => "r10d",
            RealRegister::R11 => "r11d",
            RealRegister::R12 => "r12d",
            RealRegister::R13 => "r13d",
            RealRegister::R14 => "r14d",
            RealRegister::R15 => "r15d"
        }
    }

    pub fn name_word(&self) -> &'static str {
        match *self {
            RealRegister::None => "???",
            RealRegister::Rax => "ax",
            RealRegister::Rbx => "bx",
            RealRegister::Rcx => "cx",
            RealRegister::Rdx => "dx",
            RealRegister::Rbp => "bp",
            RealRegister::Rsp => "sp",
            RealRegister::Rsi => "si",
            RealRegister::Rdi => "di",
            RealRegister::R8 => "r8w",
            RealRegister::R9 => "r9w",
            RealRegister::R10 => "r10w",
            RealRegister::R11 => "r11w",
            RealRegister::R12 => "r12w",
            RealRegister::R13 => "r13w",
            RealRegister::R14 => "r14w",
            RealRegister::R15 => "r15w"
        }
    }

    pub fn name_byte(&self) -> &'static str {
        match *self {
            RealRegister::None => "???",
            RealRegister::Rax => "al",
            RealRegister::Rbx => "bl",
            RealRegister::Rcx => "cl",
            RealRegister::Rdx => "dl",
            RealRegister::Rbp => "bpl",
            RealRegister::Rsp => "spl",
            RealRegister::Rsi => "sil",
            RealRegister::Rdi => "dil",
            RealRegister::R8 => "r8b",
            RealRegister::R9 => "r9b",
            RealRegister::R10 => "r10b",
            RealRegister::R11 => "r11b",
            RealRegister::R12 => "r12b",
            RealRegister::R13 => "r13b",
            RealRegister::R14 => "r14b",
            RealRegister::R15 => "r15b"
        }
    }

    pub fn name(&self, size: RegisterSize) -> &'static str {
        match size {
            RegisterSize::QWord => self.name_qword(),
            RegisterSize::DWord => self.name_dword(),
            RegisterSize::Word => self.name_word(),
            RegisterSize::Byte => self.name_byte()
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SrcRegister(pub RealRegister, pub Option<IlRegister>);

impl SrcRegister {
    pub fn real(reg: RealRegister) -> SrcRegister {
        SrcRegister(reg, None)
    }

    pub fn virt(reg: IlRegister) -> SrcRegister {
        SrcRegister(RealRegister::None, Some(reg))
    }

    pub fn pretty(&self, size: RegisterSize) -> PrettySrcRegister {
        PrettySrcRegister(size, self)
    }
}

#[derive(Debug, Clone)]
pub struct PrettySrcRegister<'a>(RegisterSize, &'a SrcRegister);

impl <'a> fmt::Display for PrettySrcRegister<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let PrettySrcRegister(size, reg) = *self;

        if reg.0 != RealRegister::None {
            write!(f, "{}", reg.0.name(size))?;
        };

        match *reg {
            SrcRegister(_, Some(virt)) => {
                write!(f, "({})", virt)?;
            },
            _ => {}
        };

        Result::Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DestRegister(pub RealRegister, pub Option<IlRegister>, pub Option<IlRegister>);

impl DestRegister {
    pub fn real(reg: RealRegister) -> DestRegister {
        DestRegister(reg, None, None)
    }

    pub fn virt(src: Option<IlRegister>, dest: IlRegister) -> DestRegister {
        DestRegister(RealRegister::None, src, Some(dest))
    }

    pub fn pretty(&self, size: RegisterSize) -> PrettyDestRegister {
        PrettyDestRegister(size, self)
    }
}

#[derive(Debug, Clone)]
pub struct PrettyDestRegister<'a>(RegisterSize, &'a DestRegister);

impl <'a> fmt::Display for PrettyDestRegister<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let PrettyDestRegister(size, reg) = *self;

        if reg.0 != RealRegister::None {
            write!(f, "{}", reg.0.name(size))?;
        };

        match *reg {
            DestRegister(_, None, Some(dest)) => {
                write!(f, "({})", dest)?;
            },
            DestRegister(_, Some(src), Some(dest)) => {
                write!(f, "({}/{})", dest, src)?;
            },
            _ => {}
        };

        Result::Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct MemArg {
    pub base: Option<SrcRegister>,
    pub index: Option<SrcRegister>,
    pub scale: u8,
    pub displacement: i32
}

impl MemArg {
    pub fn is_valid_scale(scale: u8) -> bool {
        return scale == 1 || scale == 2 || scale == 4 || scale == 8;
    }

    pub const fn max_scale_shift() -> u8 {
        3
    }

    pub fn pretty(&self, size: RegisterSize) -> PrettyMemArg {
        PrettyMemArg(size, self)
    }
}

#[derive(Debug, Clone)]
pub struct PrettyMemArg<'a>(RegisterSize, &'a MemArg);

impl <'a> fmt::Display for PrettyMemArg<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let PrettyMemArg(size, mem) = *self;

        match size {
            RegisterSize::QWord => {
                write!(f, "qword ")?;
            },
            RegisterSize::DWord => {
                write!(f, "dword ")?;
            },
            RegisterSize::Word => {
                write!(f, "word ")?;
            },
            RegisterSize::Byte => {
                write!(f, "byte ")?;
            }
        };

        write!(f, "[")?;

        match (&mem.base, &mem.index, mem.scale, mem.displacement) {
            (None, None, _, displacement) => {
                write!(f, "{}", displacement as u32)?;
            },
            (Some(base), None, _, displacement) => {
                write!(f, "{}", base.pretty(RegisterSize::QWord))?;

                if displacement > 0 {
                    write!(f, " + {}", displacement)?;
                } else if displacement < 0 {
                    write!(f, " - {}", -displacement as u32)?;
                };
            },
            (None, Some(index), scale, displacement) => {
                write!(f, "{}", index.pretty(RegisterSize::QWord))?;

                if scale != 1 {
                    write!(f, " * {}", scale)?;
                };

                if displacement > 0 {
                    write!(f, " + {}", displacement)?;
                } else if displacement < 0 {
                    write!(f, " - {}", -displacement as u32)?;
                };
            },
            (Some(base), Some(index), scale, displacement) => {
                write!(f, "{} + {}", base.pretty(RegisterSize::QWord), index.pretty(RegisterSize::QWord))?;

                if scale != 1 {
                    write!(f, " * {}", scale)?;
                };

                if displacement > 0 {
                    write!(f, " + {}", displacement)?;
                } else if displacement < 0 {
                    write!(f, " - {}", -displacement as u32)?;
                };
            }
        };

        write!(f, "]")?;

        Result::Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum XDest {
    Mem(MemArg),
    Reg(DestRegister)
}

impl XDest {
    pub fn as_mem_arg_mut(&mut self) -> Option<&mut MemArg> {
        if let XDest::Mem(ref mut mem_arg) = *self {
            Some(mem_arg)
        } else {
            None
        }
    }

    pub fn pretty(&self, size: RegisterSize) -> PrettyXDest {
        PrettyXDest(size, self)
    }
}

#[derive(Debug, Clone)]
pub struct PrettyXDest<'a>(RegisterSize, &'a XDest);

impl <'a> fmt::Display for PrettyXDest<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let PrettyXDest(size, dest) = *self;
        match *dest {
            XDest::Mem(ref mem) => write!(f, "{}", mem.pretty(size)),
            XDest::Reg(ref reg) => write!(f, "{}", reg.pretty(size))
        }
    }
}

#[derive(Debug, Clone)]
pub enum XSrc {
    Mem(MemArg),
    Reg(SrcRegister),
    Imm(i64),
    ImmSym(String)
}

impl XSrc {
    pub fn as_mem_arg_mut(&mut self) -> Option<&mut MemArg> {
        if let XSrc::Mem(ref mut mem_arg) = *self {
            Some(mem_arg)
        } else {
            None
        }
    }

    pub fn pretty(&self, size: RegisterSize) -> PrettyXSrc {
        PrettyXSrc(size, self)
    }
}

#[derive(Debug, Clone)]
pub struct PrettyXSrc<'a>(RegisterSize, &'a XSrc);

impl <'a> fmt::Display for PrettyXSrc<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let PrettyXSrc(size, src) = *self;
        match *src {
            XSrc::Mem(ref mem) => write!(f, "{}", mem.pretty(size)),
            XSrc::Reg(ref reg) => write!(f, "{}", reg.pretty(size)),
            XSrc::Imm(val) => write!(f, "{}", val),
            XSrc::ImmSym(ref name) => write!(f, "{}", name)
        }
    }
}

#[derive(Debug, Clone)]
pub enum InstructionKind {
    RemovableNop,
    Label(Label),
    IMul(RegisterSize, XDest, XSrc),
    IMulI(RegisterSize, DestRegister, XSrc, i32),
    Add(RegisterSize, XDest, XSrc),
    Sub(RegisterSize, XDest, XSrc),
    And(RegisterSize, XDest, XSrc),
    Test(RegisterSize, XSrc, XSrc),
    Compare(RegisterSize, XSrc, XSrc),
    Neg(RegisterSize, XDest),
    Not(RegisterSize, XDest),
    Mov(RegisterSize, XDest, XSrc),
    Shl(RegisterSize, XDest, Option<IlRegister>),
    ShlI(RegisterSize, XDest, u8),
    XChgRR(RegisterSize, SrcRegister, SrcRegister),
    Push(RegisterSize, XSrc),
    Pop(RegisterSize, XDest),
    LoadEffectiveAddress(RegisterSize, DestRegister, MemArg),
    Jump(Label),
    JumpConditional(Condition, Label),
    SetCondition(Condition, XDest),
    Call(String),
    Ret,
    RegClear,
    RegIn(Vec<(RealRegister, IlRegister)>)
}

impl InstructionKind {
    pub fn for_srcs<F: FnMut (IlRegister) -> ()>(&self, mut f: F) {
        fn call_for_src_reg<F: FnMut (IlRegister) -> ()>(f: &mut F, src: &SrcRegister) {
            if let Some(src) = src.1 {
                f(src);
            };
        }

        fn call_for_mem_arg<F: FnMut (IlRegister) -> ()>(f: &mut F, mem: &MemArg) {
            if let Some(ref base) = mem.base {
                call_for_src_reg(f, base);
            };
            if let Some(ref index) = mem.index {
                call_for_src_reg(f, index);
            };
        }

        fn call_for_xsrc<F: FnMut (IlRegister) -> ()>(f: &mut F, src: &XSrc) {
            match src {
                XSrc::Reg(ref src) => {
                    call_for_src_reg(f, src);
                },
                XSrc::Mem(ref mem) => {
                    call_for_mem_arg(f, mem);
                },
                _ => {}
            };
        }

        fn call_for_dest_reg<F: FnMut (IlRegister) -> ()>(f: &mut F, dest: &DestRegister) {
            if let Some(src) = dest.1 {
                f(src);
            };
        }

        fn call_for_xdest<F: FnMut (IlRegister) -> ()>(f: &mut F, dest: &XDest) {
            match dest {
                XDest::Reg(ref dest) => {
                    call_for_dest_reg(f, dest);
                },
                XDest::Mem(ref mem) => {
                    call_for_mem_arg(f, mem);
                }
            };
        }

        let f = &mut f;

        match self {
            InstructionKind::RemovableNop => {},
            InstructionKind::Label(_) => {},
            InstructionKind::IMul(_, ref dest, ref src) => {
                call_for_xdest(f, dest);
                call_for_xsrc(f, src);
            },
            InstructionKind::IMulI(_, _, ref src, _) => {
                call_for_xsrc(f, src);
            },
            InstructionKind::Add(_, ref dest, ref src) => {
                call_for_xdest(f, dest);
                call_for_xsrc(f, src);
            },
            InstructionKind::Sub(_, ref dest, ref src) => {
                call_for_xdest(f, dest);
                call_for_xsrc(f, src);
            },
            InstructionKind::And(_, ref dest, ref src) => {
                call_for_xdest(f, dest);
                call_for_xsrc(f, src);
            },
            InstructionKind::Test(_, ref src1, ref src2) => {
                call_for_xsrc(f, src1);
                call_for_xsrc(f, src2);
            },
            InstructionKind::Compare(_, ref src1, ref src2) => {
                call_for_xsrc(f, src1);
                call_for_xsrc(f, src2);
            },
            InstructionKind::Neg(_, ref dest) => {
                call_for_xdest(f, dest);
            },
            InstructionKind::Not(_, ref dest) => {
                call_for_xdest(f, dest);
            },
            InstructionKind::Mov(_, ref dest, ref src) => {
                call_for_xdest(f, dest);
                call_for_xsrc(f, src);
            },
            InstructionKind::Shl(_, ref dest, ref src) => {
                call_for_xdest(f, dest);
                if let Some(src) = *src {
                    f(src);
                };
            },
            InstructionKind::ShlI(_, ref dest, _) => {
                call_for_xdest(f, dest);
            },
            InstructionKind::XChgRR(_, ref src1, ref src2) => {
                call_for_src_reg(f, src1);
                call_for_src_reg(f, src2);
            },
            InstructionKind::Push(_, ref src) => {
                call_for_xsrc(f, src);
            },
            InstructionKind::Pop(_, ref dest) => {
                call_for_xdest(f, dest);
            },
            InstructionKind::LoadEffectiveAddress(_, _, ref mem) => {
                call_for_mem_arg(f, mem);
            },
            InstructionKind::Jump(_) => {},
            InstructionKind::JumpConditional(_, _) => {},
            InstructionKind::SetCondition(_, ref dest) => {
                call_for_xdest(f, dest);
            },
            InstructionKind::Call(_) => {},
            InstructionKind::Ret => {},
            InstructionKind::RegClear => {},
            InstructionKind::RegIn(ref regs) => {
                for &(_, src) in regs {
                    f(src);
                };
            }
        }
    }

    pub fn for_dests<F: FnMut (IlRegister) -> ()>(&self, mut f: F) {
        fn call_for_dest_reg<F: FnMut (IlRegister) -> ()>(f: &mut F, dest: &DestRegister) {
            if let Some(dest) = dest.2 {
                f(dest);
            };
        }

        fn call_for_xdest<F: FnMut (IlRegister) -> ()>(f: &mut F, dest: &XDest) {
            if let XDest::Reg(ref dest) = *dest {
                call_for_dest_reg(f, dest);
            };
        }

        let f = &mut f;

        match self {
            InstructionKind::RemovableNop => {},
            InstructionKind::Label(_) => {},
            InstructionKind::IMul(_, ref dest, _) => {
                call_for_xdest(f, dest);
            },
            InstructionKind::IMulI(_, ref dest, _, _) => {
                call_for_dest_reg(f, dest);
            },
            InstructionKind::Add(_, ref dest, _) => {
                call_for_xdest(f, dest);
            },
            InstructionKind::Sub(_, ref dest, _) => {
                call_for_xdest(f, dest);
            },
            InstructionKind::And(_, ref dest, _) => {
                call_for_xdest(f, dest);
            },
            InstructionKind::Test(_, _, _) => {},
            InstructionKind::Compare(_, _, _) => {},
            InstructionKind::Neg(_, ref dest) => {
                call_for_xdest(f, dest);
            },
            InstructionKind::Not(_, ref dest) => {
                call_for_xdest(f, dest);
            },
            InstructionKind::Mov(_, ref dest, _) => {
                call_for_xdest(f, dest);
            },
            InstructionKind::Shl(_, ref dest, _) => {
                call_for_xdest(f, dest);
            },
            InstructionKind::ShlI(_, ref dest, _) => {
                call_for_xdest(f, dest);
            },
            InstructionKind::XChgRR(_, ref src1, ref src2) => {
                if let Some(dest) = src1.1 {
                    f(dest);
                };
                if let Some(dest) = src2.1 {
                    f(dest);
                };
            },
            InstructionKind::Push(_, _) => {},
            InstructionKind::Pop(_, ref dest) => {
                call_for_xdest(f, dest);
            },
            InstructionKind::LoadEffectiveAddress(_, ref dest, _) => {
                call_for_dest_reg(f, dest);
            },
            InstructionKind::Jump(_) => {},
            InstructionKind::JumpConditional(_, _) => {},
            InstructionKind::SetCondition(_, ref dest) => {
                call_for_xdest(f, dest);
            },
            InstructionKind::Call(_) => {},
            InstructionKind::Ret => {},
            InstructionKind::RegClear => {},
            InstructionKind::RegIn(_) => {}
        };
    }

    pub fn mem_arg_mut(&mut self) -> Option<&mut MemArg> {
        match *self {
            InstructionKind::RemovableNop => None,
            InstructionKind::Label(_) => None,
            InstructionKind::IMul(_, ref mut dest, ref mut src) => {
                dest.as_mem_arg_mut().or(src.as_mem_arg_mut())
            },
            InstructionKind::IMulI(_, _, ref mut src, _) => {
                src.as_mem_arg_mut()
            },
            InstructionKind::Add(_, ref mut dest, ref mut src) => {
                dest.as_mem_arg_mut().or(src.as_mem_arg_mut())
            },
            InstructionKind::Sub(_, ref mut dest, ref mut src) => {
                dest.as_mem_arg_mut().or(src.as_mem_arg_mut())
            },
            InstructionKind::And(_, ref mut dest, ref mut src) => {
                dest.as_mem_arg_mut().or(src.as_mem_arg_mut())
            },
            InstructionKind::Test(_, ref mut src1, ref mut src2) => {
                src1.as_mem_arg_mut().or(src2.as_mem_arg_mut())
            },
            InstructionKind::Compare(_, ref mut src1, ref mut src2) => {
                src1.as_mem_arg_mut().or(src2.as_mem_arg_mut())
            },
            InstructionKind::Neg(_, ref mut dest) => {
                dest.as_mem_arg_mut()
            },
            InstructionKind::Not(_, ref mut dest) => {
                dest.as_mem_arg_mut()
            },
            InstructionKind::Mov(_, ref mut dest, ref mut src) => {
                dest.as_mem_arg_mut().or(src.as_mem_arg_mut())
            },
            InstructionKind::Shl(_, ref mut dest, _) => {
                dest.as_mem_arg_mut()
            },
            InstructionKind::ShlI(_, ref mut dest, _) => {
                dest.as_mem_arg_mut()
            },
            InstructionKind::XChgRR(_, _, _) => None,
            InstructionKind::Push(_, ref mut src) => {
                src.as_mem_arg_mut()
            },
            InstructionKind::Pop(_, ref mut dest) => {
                dest.as_mem_arg_mut()
            },
            InstructionKind::LoadEffectiveAddress(_, _, ref mut mem_arg) => {
                Some(mem_arg)
            },
            InstructionKind::Jump(_) => None,
            InstructionKind::JumpConditional(_, _) => None,
            InstructionKind::SetCondition(_, _) => None,
            InstructionKind::Call(_) => None,
            InstructionKind::Ret => None,
            InstructionKind::RegClear => None,
            InstructionKind::RegIn(_) => None
        }
    }

    pub fn sets_eflags(&self) -> bool {
        match self {
            InstructionKind::RemovableNop => false,
            InstructionKind::Label(_) => true,
            InstructionKind::IMul(_, _, _) => true,
            InstructionKind::IMulI(_, _, _, _) => true,
            InstructionKind::Add(_, _, _) => true,
            InstructionKind::Sub(_, _, _) => true,
            InstructionKind::And(_, _, _) => true,
            InstructionKind::Test(_, _, _) => true,
            InstructionKind::Compare(_, _, _) => true,
            InstructionKind::Neg(_, _) => true,
            InstructionKind::Not(_, _) => false,
            InstructionKind::Mov(_, _, _) => false,
            InstructionKind::Shl(_, _, _) => true,
            InstructionKind::ShlI(_, _, _) => true,
            InstructionKind::XChgRR(_, _, _) => false,
            InstructionKind::Push(_, _) => false,
            InstructionKind::Pop(_, _) => false,
            InstructionKind::LoadEffectiveAddress(_, _, _) => false,
            InstructionKind::Jump(_) => false,
            InstructionKind::JumpConditional(_, _) => false,
            InstructionKind::SetCondition(_, _) => false,
            InstructionKind::Call(_) => true,
            InstructionKind::Ret => false,
            InstructionKind::RegClear => false,
            InstructionKind::RegIn(_) => false
        }
    }

    pub fn uses_eflags(&self) -> bool {
        match self {
            InstructionKind::RemovableNop => false,
            InstructionKind::Label(_) => false,
            InstructionKind::IMul(_, _, _) => false,
            InstructionKind::IMulI(_, _, _, _) => false,
            InstructionKind::Add(_, _, _) => false,
            InstructionKind::Sub(_, _, _) => false,
            InstructionKind::And(_, _, _) => false,
            InstructionKind::Test(_, _, _) => false,
            InstructionKind::Compare(_, _, _) => false,
            InstructionKind::Neg(_, _) => false,
            InstructionKind::Not(_, _) => false,
            InstructionKind::Mov(_, _, _) => false,
            InstructionKind::Shl(_, _, _) => false,
            InstructionKind::ShlI(_, _, _) => false,
            InstructionKind::XChgRR(_, _, _) => false,
            InstructionKind::Push(_, _) => false,
            InstructionKind::Pop(_, _) => false,
            InstructionKind::LoadEffectiveAddress(_, _, _) => false,
            InstructionKind::Jump(_) => false,
            InstructionKind::JumpConditional(_, _) => true,
            InstructionKind::SetCondition(_, _) => true,
            InstructionKind::Call(_) => false,
            InstructionKind::Ret => false,
            InstructionKind::RegClear => false,
            InstructionKind::RegIn(_) => false
        }
    }
}

impl fmt::Display for InstructionKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            InstructionKind::RemovableNop => {
                write!(f, "nop ; Removable")?;
            },
            InstructionKind::Label(label) => {
                write!(f, ".{}:", label)?;
            },
            InstructionKind::IMul(size, ref dest, ref src) => {
                write!(f, "imul {}, {}", dest.pretty(size), src.pretty(size))?;
            },
            InstructionKind::IMulI(size, ref dest, ref src, imm) => {
                write!(f, "imul {}, {}, {}", dest.pretty(size), src.pretty(size), imm)?;
            },
            InstructionKind::Add(size, ref dest, ref src) => {
                write!(f, "add {}, {}", dest.pretty(size), src.pretty(size))?;
            },
            InstructionKind::Sub(size, ref dest, ref src) => {
                write!(f, "sub {}, {}", dest.pretty(size), src.pretty(size))?;
            },
            InstructionKind::And(size, ref dest, ref src) => {
                write!(f, "and {}, {}", dest.pretty(size), src.pretty(size))?;
            },
            InstructionKind::Test(size, ref src1, ref src2) => {
                write!(f, "test {}, {}", src1.pretty(size), src2.pretty(size))?;
            },
            InstructionKind::Compare(size, ref src1, ref src2) => {
                write!(f, "cmp {}, {}", src1.pretty(size), src2.pretty(size))?;
            },
            InstructionKind::Neg(size, ref dest) => {
                write!(f, "neg {}", dest.pretty(size))?;
            },
            InstructionKind::Not(size, ref dest) => {
                write!(f, "not {}", dest.pretty(size))?;
            },
            InstructionKind::Mov(size, ref dest, ref src) => {
                write!(f, "mov {}, {}", dest.pretty(size), src.pretty(size))?;
            },
            InstructionKind::Shl(size, ref dest, ref src) => {
                write!(f, "shl {}, cl", dest.pretty(size))?;
                if let Some(src) = *src {
                    write!(f, "({})", src)?;
                };
            },
            InstructionKind::ShlI(size, ref dest, imm) => {
                write!(f, "shl {}, {}", dest.pretty(size), imm)?;
            },
            InstructionKind::XChgRR(size, ref dest1, ref dest2) => {
                write!(f, "xchg {}, {}", dest1.pretty(size), dest2.pretty(size))?;
            },
            InstructionKind::Push(size, ref src) => {
                write!(f, "push {}", src.pretty(size))?;
            },
            InstructionKind::Pop(size, ref dest) => {
                write!(f, "pop {}", dest.pretty(size))?;
            },
            InstructionKind::LoadEffectiveAddress(size, ref dest, ref src) => {
                write!(f, "lea {}, {}", dest.pretty(size), src.pretty(size))?;
            },
            InstructionKind::Jump(label) => {
                write!(f, "jmp .{}", label)?;
            },
            InstructionKind::JumpConditional(cond, label) => {
                write!(f, "j{} .{}", cond.name(), label)?;
            },
            InstructionKind::SetCondition(cond, ref dest) => {
                write!(f, "set{} {}", cond.name(), dest.pretty(RegisterSize::Byte))?;
            },
            InstructionKind::Call(ref func) => {
                write!(f, "call {}", func)?;
            },
            InstructionKind::Ret => {
                write!(f, "ret")?;
            },
            InstructionKind::RegClear => {
                write!(f, "!reg_clear")?;
            },
            InstructionKind::RegIn(ref regs) => {
                write!(f, "!reg_in {{ ")?;

                for &(real, virt) in regs.iter() {
                    write!(f, "{}:{}", real.name_qword(), virt)?;
                };

                write!(f, "}}")?;
            }
        };
        Result::Ok(())
    }
}

pub struct Instruction {
    pub node: InstructionKind,
    pub span: IlSpanId,
    pub rc: u16
}

impl Instruction {
    pub fn new(node: InstructionKind, span: IlSpanId) -> Instruction {
        Instruction { node, span, rc: 0 }
    }
}
