use std::collections::HashMap;

use super::Function;
use super::instr::*;
use crate::codegen::label::{Label, LabelAlloc};
use crate::il::*;
use crate::log::Log;

struct CodeGenContext {
    code: Vec<Instruction>,
    regs: IlRegisterAllocator,
    reg_map: IlRegisterMap,
    end_label: Label,
    block_labels: HashMap<IlBlockId, Label>
}

fn generate_src_for_operand(o: &IlOperand, _ctx: &mut CodeGenContext, _span: IlSpanId) -> XSrc {
    match *o {
        IlOperand::Const(IlConst::I1(val)) => XSrc::Imm(if val { 1 } else { 0 }),
        IlOperand::Const(IlConst::I32(val)) => XSrc::Imm(val),
        IlOperand::Register(reg) => XSrc::Reg(SrcRegister::virt(reg))
    }
}

fn generate_load_for_operand(o: &IlOperand, ctx: &mut CodeGenContext, span: IlSpanId) -> IlRegister {
    match *o {
        IlOperand::Const(IlConst::I1(val)) => {
            let val = if val { 1 } else { 0 };
            let reg = ctx.regs.allocate();
            ctx.reg_map.add_reg_info(reg, IlRegisterInfo(IlRegisterType::Temp, IlType::I1));

            ctx.code.push(Instruction::new(
                InstructionKind::Mov(
                    RegisterSize::Byte,
                    XDest::Reg(DestRegister::virt(None, reg)),
                    XSrc::Imm(val)
                ),
                span
            ));
            reg
        },
        IlOperand::Const(IlConst::I32(val)) => {
            let reg = ctx.regs.allocate();
            ctx.reg_map.add_reg_info(reg, IlRegisterInfo(IlRegisterType::Temp, IlType::I32));

            ctx.code.push(Instruction::new(
                InstructionKind::Mov(
                    RegisterSize::DWord,
                    XDest::Reg(DestRegister::virt(None, reg)),
                    XSrc::Imm(val)
                ),
                span
            ));
            reg
        },
        IlOperand::Register(reg) => reg
    }
}

fn generate_load_for_operand_into(o: &IlOperand, tgt: RealRegister, ctx: &mut CodeGenContext, span: IlSpanId) {
    let o = generate_src_for_operand(o, ctx, span);
    ctx.code.push(Instruction::new(
        InstructionKind::Mov(
            RegisterSize::DWord,
            XDest::Reg(DestRegister::real(tgt)),
            o
        ),
        span
    ));
}

fn generate_code_for_instr(instr: &IlInstruction, ctx: &mut CodeGenContext, log: &mut Log) {
    use crate::il::IlInstructionKind::*;

    let old_len = ctx.code.len();
    let span = instr.span;

    log_writeln!(log, "# {}", instr);

    match instr.node {
        Nop => {},
        Copy(tgt, ref src) => {
            let src = generate_src_for_operand(src, ctx, span);

            ctx.code.push(Instruction::new(
                InstructionKind::Mov(
                    RegisterSize::DWord,
                    XDest::Reg(DestRegister::virt(None, tgt)),
                    src
                ),
                span
            ));
        },
        NegI32(tgt, ref src) => {
            let src = generate_load_for_operand(src, ctx, span);

            ctx.code.push(Instruction::new(
                InstructionKind::Neg(
                    RegisterSize::DWord,
                    XDest::Reg(DestRegister::virt(
                        Some(src),
                        tgt
                    ))
                ),
                span
            ));
        },
        NotI1(tgt, ref src) => {
            let src = generate_load_for_operand(src, ctx, span);

            ctx.code.push(Instruction::new(
                InstructionKind::Xor(
                    RegisterSize::Byte,
                    XDest::Reg(DestRegister::virt(Some(src), tgt)),
                    XSrc::Imm(1)
                ),
                span
            ));
        },
        NotI32(tgt, ref src) => {
            let src = generate_load_for_operand(src, ctx, span);

            ctx.code.push(Instruction::new(
                InstructionKind::Not(
                    RegisterSize::DWord,
                    XDest::Reg(DestRegister::virt(
                        Some(src),
                        tgt
                    ))
                ),
                span
            ));
        },
        AddI32(tgt, ref src1, ref src2) => {
            let src1 = generate_load_for_operand(src1, ctx, span);
            let src2 = generate_src_for_operand(src2, ctx, span);

            ctx.code.push(Instruction::new(
                InstructionKind::Add(
                    RegisterSize::DWord,
                    XDest::Reg(DestRegister::virt(
                        Some(src1),
                        tgt
                    )),
                    src2
                ),
                span
            ));
        },
        SubI32(tgt, ref src1, ref src2) => {
            let src1 = generate_load_for_operand(src1, ctx, span);
            let src2 = generate_src_for_operand(src2, ctx, span);

            ctx.code.push(Instruction::new(
                InstructionKind::Sub(
                    RegisterSize::DWord,
                    XDest::Reg(DestRegister::virt(
                        Some(src1),
                        tgt
                    )),
                    src2
                ),
                span
            ));
        },
        MulI32(tgt, ref src1, ref src2) => {
            let src1 = generate_load_for_operand(src1, ctx, span);
            let src2 = generate_src_for_operand(src2, ctx, span);

            ctx.code.push(Instruction::new(
                InstructionKind::IMul(
                    RegisterSize::DWord,
                    XDest::Reg(DestRegister::virt(
                        Some(src1),
                        tgt
                    )),
                    src2
                ),
                span
            ));
        },
        ShlI32(tgt, ref src1, ref src2) => {
            let src1 = generate_load_for_operand(src1, ctx, span);

            match *src2 {
                IlOperand::Const(IlConst::I32(imm)) => {
                    ctx.code.push(Instruction::new(
                        InstructionKind::ShlI(
                            RegisterSize::DWord,
                            XDest::Reg(DestRegister::virt(
                                Some(src1),
                                tgt
                            )),
                            imm as u8
                        ),
                        span
                    ));
                },
                _ => {
                    let src2 = generate_load_for_operand(src2, ctx, span);
                    ctx.code.push(Instruction::new(
                        InstructionKind::Shl(
                            RegisterSize::DWord,
                            XDest::Reg(DestRegister::virt(
                                Some(src1),
                                tgt
                            )),
                            Some(src2)
                        ),
                        span
                    ));
                }
            }
        },
        EqI1(tgt, ref src1, IlOperand::Const(IlConst::I1(false))) => {
            let src1 = generate_load_for_operand(src1, ctx, span);

            ctx.code.push(Instruction::new(
                InstructionKind::Test(
                    RegisterSize::Byte,
                    XSrc::Reg(SrcRegister::virt(src1)),
                    XSrc::Reg(SrcRegister::virt(src1))
                ),
                span
            ));
            ctx.code.push(Instruction::new(
                InstructionKind::SetCondition(
                    Condition::Equal,
                    XDest::Reg(DestRegister::virt(None, tgt))
                ),
                span
            ));
        },
        EqI1(tgt, ref src1, ref src2) => {
            let src1 = generate_src_for_operand(src1, ctx, span);
            let src2 = generate_src_for_operand(src2, ctx, span);

            ctx.code.push(Instruction::new(
                InstructionKind::Compare(
                    RegisterSize::Byte,
                    src1,
                    src2
                ),
                span
            ));
            ctx.code.push(Instruction::new(
                InstructionKind::SetCondition(
                    Condition::Equal,
                    XDest::Reg(DestRegister::virt(None, tgt))
                ),
                span
            ));
        },
        EqI32(tgt, ref src1, IlOperand::Const(IlConst::I32(0))) => {
            let src1 = generate_load_for_operand(src1, ctx, span);

            ctx.code.push(Instruction::new(
                InstructionKind::Test(
                    RegisterSize::DWord,
                    XSrc::Reg(SrcRegister::virt(src1)),
                    XSrc::Reg(SrcRegister::virt(src1))
                ),
                span
            ));
            ctx.code.push(Instruction::new(
                InstructionKind::SetCondition(
                    Condition::Equal,
                    XDest::Reg(DestRegister::virt(None, tgt))
                ),
                span
            ));
        },
        EqI32(tgt, ref src1, ref src2) => {
            let src1 = generate_src_for_operand(src1, ctx, span);
            let src2 = generate_src_for_operand(src2, ctx, span);

            ctx.code.push(Instruction::new(
                InstructionKind::Compare(
                    RegisterSize::DWord,
                    src1,
                    src2
                ),
                span
            ));
            ctx.code.push(Instruction::new(
                InstructionKind::SetCondition(
                    Condition::Equal,
                    XDest::Reg(DestRegister::virt(None, tgt))
                ),
                span
            ));
        },
        NeI1(tgt, ref src1, IlOperand::Const(IlConst::I1(false))) => {
            let src1 = generate_load_for_operand(src1, ctx, span);

            ctx.code.push(Instruction::new(
                InstructionKind::Test(
                    RegisterSize::Byte,
                    XSrc::Reg(SrcRegister::virt(src1)),
                    XSrc::Reg(SrcRegister::virt(src1))
                ),
                span
            ));
            ctx.code.push(Instruction::new(
                InstructionKind::SetCondition(
                    Condition::NotEqual,
                    XDest::Reg(DestRegister::virt(None, tgt))
                ),
                span
            ));
        },
        NeI1(tgt, ref src1, ref src2) => {
            let src1 = generate_src_for_operand(src1, ctx, span);
            let src2 = generate_src_for_operand(src2, ctx, span);

            ctx.code.push(Instruction::new(
                InstructionKind::Compare(
                    RegisterSize::Byte,
                    src1,
                    src2
                ),
                span
            ));
            ctx.code.push(Instruction::new(
                InstructionKind::SetCondition(
                    Condition::NotEqual,
                    XDest::Reg(DestRegister::virt(None, tgt))
                ),
                span
            ));
        },
        NeI32(tgt, ref src1, IlOperand::Const(IlConst::I32(0))) => {
            let src1 = generate_load_for_operand(src1, ctx, span);

            ctx.code.push(Instruction::new(
                InstructionKind::Test(
                    RegisterSize::DWord,
                    XSrc::Reg(SrcRegister::virt(src1)),
                    XSrc::Reg(SrcRegister::virt(src1))
                ),
                span
            ));
            ctx.code.push(Instruction::new(
                InstructionKind::SetCondition(
                    Condition::NotEqual,
                    XDest::Reg(DestRegister::virt(None, tgt))
                ),
                span
            ));
        },
        NeI32(tgt, ref src1, ref src2) => {
            let src1 = generate_src_for_operand(src1, ctx, span);
            let src2 = generate_src_for_operand(src2, ctx, span);

            ctx.code.push(Instruction::new(
                InstructionKind::Compare(
                    RegisterSize::DWord,
                    src1,
                    src2
                ),
                span
            ));
            ctx.code.push(Instruction::new(
                InstructionKind::SetCondition(
                    Condition::NotEqual,
                    XDest::Reg(DestRegister::virt(None, tgt))
                ),
                span
            ));
        },
        PrintI32(_) => unimplemented!(),
        Call(_, _, _, _) => unimplemented!()
    };

    for instr_out in ctx.code[old_len..].iter() {
        log_writeln!(log, "{}", instr_out.pretty());
    };

    log_writeln!(log);
}

fn generate_code_for_end_instr(instr: &IlEndingInstruction, ctx: &mut CodeGenContext, log: &mut Log) {
    use crate::il::IlEndingInstructionKind::*;

    let old_len = ctx.code.len();
    let span = instr.span;

    log_writeln!(log, "# {}", instr);

    match instr.node {
        Nop => {},
        Jump(tgt) => {
            let label = ctx.block_labels[&tgt];

            ctx.code.push(Instruction::new(
                InstructionKind::Jump(label),
                instr.span
            ));
        },
        JumpNonZeroI1(tgt, ref cond) => {
            let label = ctx.block_labels[&tgt];
            let cond = generate_load_for_operand(cond, ctx, span);

            ctx.code.push(Instruction::new(
                InstructionKind::Test(
                    RegisterSize::Byte,
                    XSrc::Reg(SrcRegister::virt(cond)),
                    XSrc::Reg(SrcRegister::virt(cond))
                ),
                instr.span
            ));
            ctx.code.push(Instruction::new(
                InstructionKind::JumpConditional(
                    Condition::NotEqual,
                    label
                ),
                instr.span
            ));
        },
        JumpZeroI1(tgt, ref cond) => {
            let label = ctx.block_labels[&tgt];
            let cond = generate_load_for_operand(cond, ctx, span);

            ctx.code.push(Instruction::new(
                InstructionKind::Test(
                    RegisterSize::Byte,
                    XSrc::Reg(SrcRegister::virt(cond)),
                    XSrc::Reg(SrcRegister::virt(cond))
                ),
                instr.span
            ));
            ctx.code.push(Instruction::new(
                InstructionKind::JumpConditional(
                    Condition::Equal,
                    label
                ),
                instr.span
            ));
        },
        Return(ref val) => {
            if !matches!(*val, IlOperand::Register(reg) if ctx.reg_map.get_reg_info(reg).1 == IlType::Void) {
                generate_load_for_operand_into(val, RealRegister::Rax, ctx, span);
            };
            ctx.code.push(Instruction::new(InstructionKind::RegClear, instr.span));
            ctx.code.push(Instruction::new(
                InstructionKind::Jump(ctx.end_label),
                instr.span
            ));
        }
    };

    for instr_out in ctx.code[old_len..].iter() {
        log_writeln!(log, "{}", instr_out.pretty());
    };

    log_writeln!(log);
}

fn generate_code_for_block(block: &IlBlock, ctx: &mut CodeGenContext, log: &mut Log) {
    ctx.code.push(Instruction::new(
        InstructionKind::Label(ctx.block_labels[&block.id]),
        IlSpanId::dummy()
    ));

    log_writeln!(log, "# Block {}", block.id);
    log_writeln!(log, "{}\n", ctx.code.last().unwrap().pretty());

    for instr in block.instrs.iter() {
        generate_code_for_instr(instr, ctx, log);
    };
    generate_code_for_end_instr(&block.end_instr, ctx, log);
}

pub fn generate_code(func: &IlFunction, log: &mut Log) -> Function {
    log_writeln!(log, "\n===== INSTRUCTION SELECTION FOR {} =====\n", func.sym);

    let mut labels = LabelAlloc::new();
    let block_labels: HashMap<_, _> = func.block_order.iter().map(|&id| {
        (id, labels.alloc())
    }).collect();
    let end_label = labels.alloc();
    let mut ctx = CodeGenContext {
        code: vec![],
        regs: func.reg_alloc.clone(),
        reg_map: func.reg_map.clone(),
        end_label,
        block_labels
    };

    for &id in func.block_order.iter() {
        generate_code_for_block(&func.blocks[&id], &mut ctx, log);
    };

    ctx.code.push(Instruction::new(InstructionKind::Label(end_label), IlSpanId::dummy()));

    Function {
        sym: func.sym,
        instrs: ctx.code,
        regs: ctx.regs,
        reg_map: ctx.reg_map
    }
}
