use std::collections::HashMap;
use std::io::{self, Write};

use crate::il::{IlFunction, IlRegisterType, IlSpanId};
use crate::sym::SymDefTable;
use crate::types::{PrettyType, Type, TypeId, TypeTable};
use super::Function;

pub struct WriteContext<'a> {
    pub files: Vec<String>,
    pub debug_strs: Vec<String>,
    pub sym_defs: &'a SymDefTable,
    pub types: &'a TypeTable
}

impl <'a> WriteContext<'a> {
    pub fn new(sym_defs: &'a SymDefTable, types: &'a TypeTable) -> WriteContext<'a> {
        WriteContext {
            files: vec![],
            debug_strs: vec![],
            sym_defs,
            types
        }
    }

    pub fn add_file<T: Write>(&mut self, filename: &str, out: &mut T) -> io::Result<usize> {
        Result::Ok(if let Some((i, _)) = self.files.iter().enumerate().filter(|&(_, f)| f == filename).next() {
            i + 1
        } else {
            writeln!(out, ".file {} \"{}\"", self.files.len() + 1, filename)?;

            self.files.push(filename.to_owned());
            self.files.len()
        })
    }

    pub fn add_debug_str(&mut self, debug_str: &str) -> usize {
        if let Some((i, _)) = self.debug_strs.iter().enumerate().filter(|&(_, s)| s == debug_str).next() {
            i
        } else {
            self.debug_strs.push(debug_str.to_owned());
            self.debug_strs.len() - 1
        }
    }
}

pub fn write_start_to_file<T: Write>(out: &mut T, ctx: &mut WriteContext) -> io::Result<()> {
    writeln!(out, ".intel_syntax noprefix")?;

    writeln!(out, ".section .debug_abbrev,\"\",@progbits")?;

    // ABBREVIATION 0x01
    writeln!(out, "  .byte 0x01")?;

    //   DW_TAG_compile_unit
    writeln!(out, "  .byte 0x11")?;

    //   DW_CHILDREN_yes
    writeln!(out, "  .byte 1")?;

    //   DW_AT_producer
    //     DW_FORM_strp
    writeln!(out, "  .byte 0x25")?;
    writeln!(out, "  .byte 0x0e")?;

    //   DW_AT_name
    //     DW_FORM_strp
    writeln!(out, "  .byte 0x03")?;
    writeln!(out, "  .byte 0x0e")?;

    //   DW_AT_stmt_list
    //     DW_FORM_sec_offset
    writeln!(out, "  .byte 0x10")?;
    writeln!(out, "  .byte 0x17")?;

    //   DW_AT_low_pc
    //     DW_FORM_addr
    writeln!(out, "  .byte 0x11")?;
    writeln!(out, "  .byte 0x01")?;

    //   DW_AT_high_pc
    //     DW_FORM_data4
    writeln!(out, "  .byte 0x12")?;
    writeln!(out, "  .byte 0x06")?;

    // END OF ABBREVIATION 0x01
    writeln!(out, "  .byte 0")?;
    writeln!(out, "  .byte 0")?;

    // ABBREVIATION 0x02
    writeln!(out, "  .byte 0x02")?;

    //   DW_TAG_base_type
    writeln!(out, "  .byte 0x24")?;

    //   DW_CHILDREN_no
    writeln!(out, "  .byte 0")?;

    //   DW_AT_name
    //     DW_FORM_strp
    writeln!(out, "  .byte 0x03")?;
    writeln!(out, "  .byte 0x0e")?;

    //   DW_AT_encoding
    //     DW_FORM_data1
    writeln!(out, "  .byte 0x3e")?;
    writeln!(out, "  .byte 0x0b")?;

    // DW_AT_byte_size
    writeln!(out, "  .byte 0x0b")?;
    writeln!(out, "  .byte 0x0b")?;

    // END OF ABBREVIATION 0x02
    writeln!(out, "  .byte 0")?;
    writeln!(out, "  .byte 0")?;

    // ABBREVIATION 0x03
    writeln!(out, "  .byte 0x03")?;

    //   DW_TAG_unspecified_type
    writeln!(out, "  .byte 0x3b")?;

    //   DW_CHILDREN_no
    writeln!(out, "  .byte 0")?;

    //   DW_AT_name
    //     DW_FORM_strp
    writeln!(out, "  .byte 0x03")?;
    writeln!(out, "  .byte 0x0e")?;

    // END OF ABBREVIATION 0x03
    writeln!(out, "  .byte 0")?;
    writeln!(out, "  .byte 0")?;

    // ABBREVIATION 0x04
    writeln!(out, "  .byte 0x04")?;

    //   DW_TAG_typedef
    writeln!(out, "  .byte 0x16")?;

    //   DW_CHILDREN_no
    writeln!(out, "  .byte 0")?;

    //   DW_AT_name
    //     DW_FORM_strp
    writeln!(out, "  .byte 0x03")?;
    writeln!(out, "  .byte 0x0e")?;

    //   DW_AT_type
    //     DW_FORM_ref4
    writeln!(out, "  .byte 0x49")?;
    writeln!(out, "  .byte 0x13")?;

    // END OF ABBREVIATION 0x04
    writeln!(out, "  .byte 0")?;
    writeln!(out, "  .byte 0")?;

    // ABBREVIATION 0x05
    writeln!(out, "  .byte 0x05")?;

    //   DW_TAG_subprogram
    writeln!(out, "  .byte 0x2e")?;

    //   DW_CHILDREN_yes
    writeln!(out, "  .byte 1")?;

    //   DW_AT_name
    //     DW_FORM_strp
    writeln!(out, "  .byte 0x03")?;
    writeln!(out, "  .byte 0x0e")?;

    //   DW_AT_type
    //     DW_FORM_ref4
    writeln!(out, "  .byte 0x49")?;
    writeln!(out, "  .byte 0x13")?;

    //   DW_AT_low_pc
    //     DW_FORM_addr
    writeln!(out, "  .byte 0x11")?;
    writeln!(out, "  .byte 0x01")?;

    //   DW_AT_high_pc
    //     DW_FORM_data4
    writeln!(out, "  .byte 0x12")?;
    writeln!(out, "  .byte 0x06")?;

    // END OF ABBREVIATION 0x05
    writeln!(out, "  .byte 0")?;
    writeln!(out, "  .byte 0")?;

    // ABBREVIATION 0x06
    writeln!(out, "  .byte 0x06")?;

    //   DW_TAG_formal_parameter
    writeln!(out, "  .byte 0x05")?;

    //   DW_CHILDREN_no
    writeln!(out, "  .byte 0")?;

    //   DW_AT_name
    //     DW_FORM_strp
    writeln!(out, "  .byte 0x03")?;
    writeln!(out, "  .byte 0x0e")?;

    //   DW_AT_type
    //     DW_FORM_ref4
    writeln!(out, "  .byte 0x49")?;
    writeln!(out, "  .byte 0x13")?;

    // END OF ABBREVIATION 0x06
    writeln!(out, "  .byte 0")?;
    writeln!(out, "  .byte 0")?;

    // ABBREVIATION 0x07
    writeln!(out, "  .byte 0x07")?;

    //   DW_TAG_inlined_subroutine
    writeln!(out, "  .byte 0x1d")?;

    //   DW_CHILDREN_yes
    writeln!(out, "  .byte 1")?;

    //   DW_AT_abstract_origin
    //     DW_FORM_ref4
    writeln!(out, "  .byte 0x31")?;
    writeln!(out, "  .byte 0x13")?;

    //   DW_AT_call_file
    //     DW_FORM_data1
    writeln!(out, "  .byte 0x58")?;
    writeln!(out, "  .byte 0x0b")?;

    //   DW_AT_call_line
    //     DW_FORM_data4
    writeln!(out, "  .byte 0x59")?;
    writeln!(out, "  .byte 0x06")?;

    //   DW_AT_call_column
    //     DW_FORM_data4
    writeln!(out, "  .byte 0x57")?;
    writeln!(out, "  .byte 0x06")?;

    //   DW_AT_ranges
    //     DW_FORM_sec_offset
    writeln!(out, "  .byte 0x55")?;
    writeln!(out, "  .byte 0x17")?;

    // END OF ABBREVIATION 0x07
    writeln!(out, "  .byte 0")?;
    writeln!(out, "  .byte 0")?;

    // END OF ABBREVIATIONS
    writeln!(out, "  .byte 0")?;

    writeln!(out)?;
    writeln!(out, ".section .debug_info,\"\",@progbits")?;
    writeln!(out, ".Ldebug_info_start:")?;
    writeln!(out, "  .long .Ldebug_info_end-.Ldebug_info_start-4")?;
    writeln!(out, "  .short 4")?;
    writeln!(out, "  .long .debug_abbrev")?;
    writeln!(out, "  .byte 8")?;

    // DW_TAG_compile_unit
    writeln!(out, "  .byte 0x01")?;

    // DW_AT_producer
    writeln!(out, "  .long .Ldebug_str_{}", ctx.add_debug_str(&format!("hootc {}", env!("CARGO_PKG_VERSION"))))?;

    // DW_AT_name
    writeln!(out, "  .long .Ldebug_str_{}", ctx.add_debug_str(&ctx.files[0].clone()))?;

    // DW_AT_stmt_list
    writeln!(out, "  .long .Ldebug_line_start")?;

    // DW_AT_low_pc
    writeln!(out, "  .quad .Lcu_text_start")?;

    // DW_AT_high_pc
    writeln!(out, "  .long .Lcu_text_end-.Lcu_text_start")?;

    writeln!(out)?;
    writeln!(out, ".section .debug_ranges,\"\",@progbits")?;
    writeln!(out, ".Ldebug_ranges_start:")?;

    writeln!(out, ".text")?;
    writeln!(out, ".Lcu_text_start:")?;

    Result::Ok(())
}

pub fn write_types_to_file<T: Write>(out: &mut T, ctx: &mut WriteContext) -> io::Result<()> {
    writeln!(out, ".section .debug_info,\"\",@progbits")?;

    for i in 0..(ctx.types.num_types()) {
        let ty = ctx.types.find_type(TypeId(i));

        writeln!(out, ".Ldebug_type_{}:", i)?;

        match ty {
            Type::Error => {
                // DW_tag_unspecified_type
                writeln!(out, "  .byte 0x03")?;

                // DW_AT_name
                writeln!(out, "  .long .Ldebug_str_{}", ctx.add_debug_str("<error type>"))?;
            },
            Type::Bool => {
                // DW_tag_base_type
                writeln!(out, "  .byte 0x02")?;

                // DW_AT_name
                writeln!(out, "  .long .Ldebug_str_{}", ctx.add_debug_str("bool"))?;

                // DW_AT_encoding
                //   DW_ATE_boolean
                writeln!(out, "  .byte 0x02")?;

                // DW_AT_byte_size
                writeln!(out, "  .byte 0x01")?;
            },
            Type::I32 => {
                // DW_tag_base_type
                writeln!(out, "  .byte 0x02")?;

                // DW_AT_name
                writeln!(out, "  .long .Ldebug_str_{}", ctx.add_debug_str("i32"))?;

                // DW_AT_encoding
                //   DW_ATE_signed
                writeln!(out, "  .byte 0x05")?;

                // DW_AT_byte_size
                writeln!(out, "  .byte 0x04")?;
            },
            Type::Tuple(ref component_tys) => match component_tys.len() {
                0 => {
                    // DW_tag_base_type
                    writeln!(out, "  .byte 0x02")?;

                    // DW_AT_name
                    writeln!(out, "  .long .Ldebug_str_{}", ctx.add_debug_str("()"))?;

                    // DW_AT_encoding
                    //   DW_ATE_unsigned
                    writeln!(out, "  .byte 0x07")?;

                    // DW_AT_byte_size
                    writeln!(out, "  .byte 0x00")?;
                },
                1 => {
                    // DW_tag_typedef
                    writeln!(out, "  .byte 0x04")?;

                    // DW_AT_name
                    writeln!(out, "  .long .Ldebug_str_{}", ctx.add_debug_str(
                        &format!("{}", PrettyType(TypeId(i), ctx.types))
                    ))?;

                    // DW_AT_type
                    writeln!(out, "  .long .Ldebug_type_{}-.Ldebug_info_start", component_tys[0].0)?;
                },
                _ => {
                    // TODO: Implement this
                    // DW_tag_unspecified_type
                    writeln!(out, "  .byte 0x03")?;

                    // DW_AT_name
                    writeln!(out, "  .long .Ldebug_str_{}", ctx.add_debug_str(
                        &format!("{}", PrettyType(TypeId(i), ctx.types))
                    ))?;
                }
            },
            Type::Func(_, _) => {
                // TODO: Implement this
                // DW_tag_unspecified_type
                writeln!(out, "  .byte 0x03")?;

                // DW_AT_name
                writeln!(out, "  .long .Ldebug_str_{}", ctx.add_debug_str(
                    &format!("{}", PrettyType(TypeId(i), ctx.types))
                ))?;
            },
            Type::FuncKnown(_, _) => {
                // TODO: Implement this
                // DW_tag_unspecified_type
                writeln!(out, "  .byte 0x03")?;

                // DW_AT_name
                writeln!(out, "  .long .Ldebug_str_{}", ctx.add_debug_str(
                    &format!("{}", PrettyType(TypeId(i), ctx.types))
                ))?;
            },
            Type::Undecided(_) => {
                // DW_tag_unspecified_type
                writeln!(out, "  .byte 0x03")?;

                // DW_AT_name
                writeln!(out, "  .long .Ldebug_str_{}", ctx.add_debug_str(
                    &format!("{}", PrettyType(TypeId(i), ctx.types))
                ))?;
            }
        };
    };

    Result::Ok(())
}

#[derive(Debug, Clone)]
struct InlineSection {
    pub ranges: Vec<u32>,
    pub inline_site: IlSpanId,
    pub subsections: HashMap<IlSpanId, InlineSection>
}

impl InlineSection {
    fn new(inline_site: IlSpanId) -> InlineSection {
        InlineSection { ranges: vec![], inline_site, subsections: HashMap::new() }
    }

    fn write_ranges<T: Write>(&self, out: &mut T, label_prefix: &str) -> io::Result<()> {
        writeln!(out, ".{}debug_ranges_{}:", label_prefix, self.inline_site)?;

        for &range in self.ranges.iter() {
            writeln!(out, "  .quad .{}inl{}_start-.Lcu_text_start", label_prefix, range)?;
            writeln!(out, "  .quad .{}inl{}_end-.Lcu_text_start", label_prefix, range)?;
        };

        writeln!(out, "  .quad 0")?;
        writeln!(out, "  .quad 0")?;

        for (_, subsection) in self.subsections.iter() {
            subsection.write_ranges(out, label_prefix)?;
        };

        Result::Ok(())
    }

    fn write_inlined_subroutine<T: Write>(&self, out: &mut T, label_prefix: &str, il: &IlFunction, ctx: &mut WriteContext) -> io::Result<()> {
        let span = il.spans.get(self.inline_site);
        let inlined = ctx.sym_defs.find(span.2.unwrap());

        // DW_TAG_inlined_subroutine
        writeln!(out, "  .byte 0x07")?;

        // DW_AT_abstract_origin
        writeln!(out, "  .long .Ldebug_info_{}-.Ldebug_info_start", inlined.name)?;

        // DW_AT_call_file
        writeln!(out, "  .byte 1")?;

        // DW_AT_call_line
        writeln!(out, "  .long {}", span.0.lo.line)?;

        // DW_AT_call_column
        writeln!(out, "  .long {}", span.0.lo.col)?;

        // DW_AT_ranges
        writeln!(out, "  .long .{}debug_ranges_{}", label_prefix, self.inline_site)?;

        for (_, subsection) in self.subsections.iter() {
            subsection.write_inlined_subroutine(out, label_prefix, il, ctx)?;
        };

        writeln!(out, "  .byte 0")?;

        Result::Ok(())
    }
}

#[derive(Debug, Clone)]
struct InlineSectionBuilder {
    pub next_range: u32,
    pub range_stack: Vec<(IlSpanId, u32)>,
    pub sections: HashMap<IlSpanId, InlineSection>
}

fn finish_ranges<T: Write>(range_stack: &mut Vec<(IlSpanId, u32)>, out: &mut T, label_prefix: &str, level: usize) -> io::Result<()> {
    while range_stack.len() > level {
        let (_, range) = range_stack.pop().unwrap();
        writeln!(out, ".{}inl{}_end:", label_prefix, range)?;
    };

    Result::Ok(())
}

impl InlineSectionBuilder {
    fn new() -> InlineSectionBuilder {
        InlineSectionBuilder { next_range: 0, range_stack: vec![], sections: HashMap::new() }
    }

    fn move_to_site<T: Write>(&mut self, out: &mut T, label_prefix: &str, mut span: IlSpanId, il: &IlFunction) -> io::Result<()> {
        let mut site_stack = vec![];

        loop {
            span = il.spans.get(span).1;
            if span.0 == !0 {
                break;
            };

            assert!(il.spans.get(span).2.is_some());

            site_stack.push(span);
        };

        site_stack.reverse();

        if let Some((first_mismatch, _)) = site_stack.iter().zip(self.range_stack.iter()).enumerate().filter(|&(_, (site1, (site2, _)))| site1 != site2).next() {
            self.finish(out, label_prefix, first_mismatch)?;
        } else if self.range_stack.len() > site_stack.len() {
            self.finish(out, label_prefix, site_stack.len())?;
        };

        if site_stack.len() == self.range_stack.len() {
            return Result::Ok(());
        };

        let mut section = self.sections.entry(site_stack[0]).or_insert_with(|| InlineSection::new(site_stack[0]));

        if self.range_stack.is_empty() {
            let range = self.next_range;
            self.next_range += 1;

            writeln!(out, ".{}inl{}_start:", label_prefix, range)?;

            self.range_stack.push((site_stack[0], range));
            section.ranges.push(range);
        };

        for i in 1..(self.range_stack.len()) {
            section = section.subsections.get_mut(&site_stack[i]).unwrap();
        };

        for i in (self.range_stack.len())..(site_stack.len()) {
            section = section.subsections.entry(site_stack[i]).or_insert_with(|| InlineSection::new(site_stack[i]));

            let range = self.next_range;
            self.next_range += 1;

            writeln!(out, ".{}inl{}_start:", label_prefix, range)?;

            self.range_stack.push((site_stack[i], range));
            section.ranges.push(range);
        };

        Result::Ok(())
    }

    fn finish<T: Write>(&mut self, out: &mut T, label_prefix: &str, level: usize) -> io::Result<()> {
        finish_ranges(&mut self.range_stack, out, label_prefix, level)
    }
}

pub fn write_fn_to_file<T: Write>(out: &mut T, func: &Function, il: &IlFunction, fileno: usize, ctx: &mut WriteContext) -> io::Result<()> {
    let mut last_span_id = IlSpanId::dummy();
    let mut inline_builder = InlineSectionBuilder::new();
    let func_name = &ctx.sym_defs.find(il.sym).name;

    writeln!(out)?;
    writeln!(out, ".text")?;
    writeln!(out, ".globl {}", func_name)?;
    writeln!(out, ".type {},@function", func_name)?;
    writeln!(out, "{}:", func_name)?;

    let label_prefix = format!("L_{}__", func_name);

    for i in func.instrs.iter() {
        if i.span != last_span_id && i.span.0 != !0 {
            last_span_id = i.span;

            let span = il.spans.get(i.span).0;
            writeln!(out, ".loc {} {} {}", fileno, span.lo.line, span.lo.col)?;

            inline_builder.move_to_site(out, &label_prefix, i.span, il)?;
        };

        if !i.node.is_label() {
            write!(out, "  ")?;
        };

        writeln!(out, "{}", i.pretty(&label_prefix))?;
    };

    inline_builder.finish(out, &label_prefix, 0)?;

    writeln!(out, ".{}Lend_func:", label_prefix)?;
    writeln!(out, ".size {}, .{}Lend_func-{}", func_name, label_prefix, func_name)?;

    if !inline_builder.sections.is_empty() {
        writeln!(out)?;
        writeln!(out, ".section .debug_ranges,\"\",@progbits")?;

        for (_, section) in inline_builder.sections.iter() {
            section.write_ranges(out, &label_prefix)?;
        };
    };

    writeln!(out)?;
    writeln!(out, ".section .debug_info,\"\",@progbits")?;
    writeln!(out, ".Ldebug_info_{}:", func_name)?;

    let sym = ctx.sym_defs.find(il.sym);
    let return_ty = match *ctx.types.find_type(sym.ty) {
        Type::FuncKnown(_, fn_ty) => match *ctx.types.find_type(fn_ty) {
            Type::Func(return_ty, _) => return_ty,
            _ => unreachable!()
        },
        _ => unreachable!()
    };

    let param_syms: Vec<_> = func.reg_map.params().iter().map(|&r| {
        let sym = match func.reg_map.get_reg_info(r).0 {
            IlRegisterType::Param(sym, _) => sym,
            _ => unreachable!()
        };

        ctx.sym_defs.find(sym)
    }).collect();

    // DW_TAG_subprogram
    writeln!(out, "  .byte 0x05")?;

    // DW_AT_name
    writeln!(out, "  .long .Ldebug_str_{}", ctx.add_debug_str(func_name))?;

    // DW_AT_type
    writeln!(out, "  .long .Ldebug_type_{}-.Ldebug_info_start", return_ty.0)?;

    // DW_AT_low_pc
    writeln!(out, "  .quad {}", func_name)?;

    // DW_AT_high_pc
    writeln!(out, "  .long .{}Lend_func-{}", label_prefix, func_name)?;

    for param_sym in param_syms {
        // DW_TAG_formal_parameter
        writeln!(out, "  .byte 0x06")?;

        // DW_AT_name
        writeln!(out, "  .long .Ldebug_str_{}", ctx.add_debug_str(&param_sym.name))?;

        // DW_AT_type
        writeln!(out, "  .long .Ldebug_type_{}-.Ldebug_info_start", param_sym.ty.0)?;
    };

    for (_, section) in inline_builder.sections.iter() {
        section.write_inlined_subroutine(out, &label_prefix, il, ctx)?;
    };

    writeln!(out, "  .byte 0")?;

    Result::Ok(())
}

pub fn write_end_to_file<T: Write>(out: &mut T, ctx: &mut WriteContext) -> io::Result<()> {
    writeln!(out)?;
    writeln!(out, ".text")?;
    writeln!(out, ".Lcu_text_end:")?;

    writeln!(out)?;
    writeln!(out, ".section .debug_info,\"\",@progbits")?;
    writeln!(out, "  .byte 0")?;
    writeln!(out, ".Ldebug_info_end:")?;

    writeln!(out)?;
    writeln!(out, ".section .debug_str,\"MS\",@progbits,1")?;
    for (i, debug_str) in ctx.debug_strs.iter().enumerate() {
        writeln!(out, ".Ldebug_str_{}:", i)?;
        writeln!(out, "  .asciz \"{}\"", debug_str)?;
    };

    writeln!(out)?;
    writeln!(out, ".section .debug_line,\"\",@progbits")?;
    writeln!(out, ".Ldebug_line_start:")?;
    Result::Ok(())
}
