use std::collections::HashMap;
use std::io::Write;

use super::instr::*;
use crate::codegen::label::{Label, LabelAlloc};
use crate::il::*;

struct CodeGenContext {
    code: Vec<Instruction>,
    regs: IlRegisterAllocator,
    end_label: Label,
    block_labels: HashMap<IlBlockId, Label>
}

fn generate_src_for_operand(o: &IlOperand, _ctx: &mut CodeGenContext, _span: IlSpanId) -> XSrc {
    match *o {
        IlOperand::Const(IlConst::I32(val)) => XSrc::Imm(val as i64),
        IlOperand::Register(reg) => XSrc::Reg(SrcRegister::virt(reg))
    }
}

fn generate_load_for_operand(o: &IlOperand, ctx: &mut CodeGenContext, span: IlSpanId) -> IlRegister {
    match *o {
        IlOperand::Const(IlConst::I32(val)) => {
            let reg = ctx.regs.allocate();
            ctx.code.push(Instruction::new(
                InstructionKind::Mov(
                    RegisterSize::DWord,
                    XDest::Reg(DestRegister::virt(None, reg)),
                    XSrc::Imm(val as i64)
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

fn generate_code_for_instr(instr: &IlInstruction, ctx: &mut CodeGenContext, w: &mut Write) {
    use crate::il::IlInstructionKind::*;

    let old_len = ctx.code.len();
    let span = instr.span;

    writeln!(w, "; {}", instr).unwrap();

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
        LogicNotI32(tgt, ref src) => {
            let src = generate_load_for_operand(src, ctx, span);

            ctx.code.push(Instruction::new(
                InstructionKind::Test(
                    RegisterSize::DWord,
                    XSrc::Reg(SrcRegister::virt(src)),
                    XSrc::Reg(SrcRegister::virt(src))
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
            ctx.code.push(Instruction::new(
                InstructionKind::And(
                    RegisterSize::DWord,
                    XDest::Reg(DestRegister::virt(Some(tgt), tgt)),
                    XSrc::Imm(1)
                ),
                instr.span
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
                instr.span
            ));
            ctx.code.push(Instruction::new(
                InstructionKind::And(
                    RegisterSize::DWord,
                    XDest::Reg(DestRegister::virt(Some(tgt), tgt)),
                    XSrc::Imm(1)
                ),
                instr.span
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
                instr.span
            ));
            ctx.code.push(Instruction::new(
                InstructionKind::And(
                    RegisterSize::DWord,
                    XDest::Reg(DestRegister::virt(Some(tgt), tgt)),
                    XSrc::Imm(1)
                ),
                instr.span
            ));
        },
        PrintI32(_) => unimplemented!(),
        Call(_, _, _, _) => unimplemented!()
    };

    for instr_out in ctx.code[old_len..].iter() {
        writeln!(w, "{}", instr_out.node).unwrap();
    };

    writeln!(w).unwrap();
}

fn generate_code_for_end_instr(instr: &IlEndingInstruction, ctx: &mut CodeGenContext, w: &mut Write) {
    use crate::il::IlEndingInstructionKind::*;

    let old_len = ctx.code.len();
    let span = instr.span;

    writeln!(w, "; {}", instr).unwrap();

    match instr.node {
        Nop => {},
        Jump(tgt) => {
            let label = ctx.block_labels[&tgt];

            ctx.code.push(Instruction::new(
                InstructionKind::Jump(label),
                instr.span
            ));
        },
        JumpNonZero(tgt, ref cond) => {
            let label = ctx.block_labels[&tgt];
            let cond = generate_load_for_operand(cond, ctx, span);

            ctx.code.push(Instruction::new(
                InstructionKind::Test(
                    RegisterSize::DWord,
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
        JumpZero(tgt, ref cond) => {
            let label = ctx.block_labels[&tgt];
            let cond = generate_load_for_operand(cond, ctx, span);

            ctx.code.push(Instruction::new(
                InstructionKind::Test(
                    RegisterSize::DWord,
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
            generate_load_for_operand_into(val, RealRegister::Rax, ctx, span);
            ctx.code.push(Instruction::new(InstructionKind::RegClear, instr.span));
            ctx.code.push(Instruction::new(
                InstructionKind::Jump(ctx.end_label),
                instr.span
            ));
        }
    };

    for instr_out in ctx.code[old_len..].iter() {
        writeln!(w, "{}", instr_out.node).unwrap();
    };

    writeln!(w).unwrap();
}

fn generate_code_for_block(block: &IlBlock, ctx: &mut CodeGenContext, w: &mut Write) {
    ctx.code.push(Instruction::new(
        InstructionKind::Label(ctx.block_labels[&block.id]),
        IlSpanId::dummy()
    ));

    writeln!(w, "; Block {}", block.id).unwrap();
    writeln!(w, "{}\n", ctx.code.last().unwrap().node).unwrap();

    for instr in block.instrs.iter() {
        generate_code_for_instr(instr, ctx, w);
    };
    generate_code_for_end_instr(&block.end_instr, ctx, w);
}

pub fn generate_code(func: &IlFunction, w: &mut Write) -> Vec<Instruction> {
    writeln!(w, "\n===== INSTRUCTION SELECTION FOR {} =====\n", func.sym).unwrap();

    let mut labels = LabelAlloc::new();
    let block_labels: HashMap<_, _> = func.block_order.iter().map(|&id| {
        (id, labels.alloc())
    }).collect();
    let end_label = labels.alloc();
    let mut ctx = CodeGenContext {
        code: vec![],
        regs: func.reg_alloc.clone(),
        end_label,
        block_labels
    };

    for &id in func.block_order.iter() {
        generate_code_for_block(&func.blocks[&id], &mut ctx, w);
    };

    ctx.code.push(Instruction::new(InstructionKind::Label(end_label), IlSpanId::dummy()));

    ctx.code
}
