use crate::il::{IlRegister, IlSpanId, IlType};
use super::instr::*;

pub trait CallingConvention {
    fn allocatable_regs(&self) -> &'static [RealRegister];
    fn nonvolatile_regs(&self) -> &'static [RealRegister];
    fn int_arg_regs(&self) -> &'static [RealRegister];
    fn int_return_regs(&self) -> &'static [RealRegister];

    fn is_frame_pointer_mandatory(&self) -> bool;
    fn frame_pointer(&self) -> RealRegister;
    fn stack_pointer(&self) -> RealRegister;

    fn stack_alignment(&self) -> u32;
    fn stack_red_zone(&self) -> u32;

    fn load_args<'a>(&self, code: &mut Vec<Instruction>, params: impl Iterator<Item=(IlRegister, &'a IlType)>) {
        let mut int_arg_regs = self.int_arg_regs();

        for (reg, ty) in params {
            match *ty {
                IlType::I32 => {
                    if int_arg_regs.is_empty() {
                        unimplemented!();
                    } else {
                        let real_reg = int_arg_regs[0];
                        int_arg_regs = &int_arg_regs[1..];

                        code.push(Instruction::new(
                            InstructionKind::Mov(
                                RegisterSize::DWord,
                                XDest::Reg(DestRegister::virt(None, reg)),
                                XSrc::Reg(SrcRegister::real(real_reg))
                            ),
                            IlSpanId::dummy()
                        ));
                    };
                },
                IlType::Addr => {
                    if int_arg_regs.is_empty() {
                        unimplemented!();
                    } else {
                        let real_reg = int_arg_regs[0];
                        int_arg_regs = &int_arg_regs[1..];

                        code.push(Instruction::new(
                            InstructionKind::Mov(
                                RegisterSize::QWord,
                                XDest::Reg(DestRegister::virt(None, reg)),
                                XSrc::Reg(SrcRegister::real(real_reg))
                            ),
                            IlSpanId::dummy()
                        ));
                    };
                },
                IlType::Void => {}
            };
        };
    }
}

pub struct SysVCallingConvention();

impl SysVCallingConvention {
    const ALLOCATABLE_REGS: &'static [RealRegister] = &[
        RealRegister::Rax,
        RealRegister::Rbx,
        RealRegister::Rcx,
        RealRegister::Rdx,
        RealRegister::Rsi,
        RealRegister::Rdi,
        RealRegister::R8,
        RealRegister::R9,
        RealRegister::R10,
        RealRegister::R11,
        RealRegister::R12,
        RealRegister::R13,
        RealRegister::R14,
        RealRegister::R15
    ];
    const NONVOLATILE_REGS: &'static [RealRegister] = &[
        RealRegister::Rbx,
        RealRegister::R12,
        RealRegister::R13,
        RealRegister::R14,
        RealRegister::R15
    ];
    const INT_ARG_REGS: &'static [RealRegister] = &[
        RealRegister::Rdi,
        RealRegister::Rsi,
        RealRegister::Rdx,
        RealRegister::Rcx,
        RealRegister::R8,
        RealRegister::R9
    ];
    const INT_RETURN_REGS: &'static [RealRegister] = &[
        RealRegister::Rax,
        RealRegister::Rdx
    ];
}

impl CallingConvention for SysVCallingConvention {
    fn allocatable_regs(&self) -> &'static [RealRegister] {
        Self::ALLOCATABLE_REGS
    }

    fn nonvolatile_regs(&self) -> &'static [RealRegister] {
        Self::NONVOLATILE_REGS
    }

    fn int_arg_regs(&self) -> &'static [RealRegister] {
        Self::INT_ARG_REGS
    }

    fn int_return_regs(&self) -> &'static [RealRegister] {
        Self::INT_RETURN_REGS
    }

    fn is_frame_pointer_mandatory(&self) -> bool {
        false
    }
    fn frame_pointer(&self) -> RealRegister {
        RealRegister::Rbp
    }

    fn stack_pointer(&self) -> RealRegister {
        RealRegister::Rsp
    }

    fn stack_alignment(&self) -> u32 {
        16
    }

    fn stack_red_zone(&self) -> u32 {
        128
    }
}
