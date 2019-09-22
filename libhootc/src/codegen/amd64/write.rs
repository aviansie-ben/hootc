use std::io::{self, Write};

use crate::il::{IlFunction, IlSpanId};
use crate::sym::SymDefTable;
use super::Function;

pub struct WriteContext<'a> {
    pub files: Vec<String>,
    pub debug_strs: Vec<String>,
    pub sym_defs: &'a SymDefTable
}

impl <'a> WriteContext<'a> {
    pub fn new(sym_defs: &'a SymDefTable) -> WriteContext<'a> {
        WriteContext {
            files: vec![],
            debug_strs: vec![],
            sym_defs
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

    // ABBREVIATION 0x01:
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

    // END OF ABBREVIATIONS
    writeln!(out, "  .byte 0")?;

    writeln!(out)?;
    writeln!(out, ".section .debug_info,\"\",@progbits")?;
    writeln!(out, "  .long .Ldebug_info_end-.Ldebug_info_start")?;
    writeln!(out, ".Ldebug_info_start:")?;
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

    writeln!(out, ".text")?;
    writeln!(out, ".Lcu_text_start:")?;

    Result::Ok(())
}

pub fn write_fn_to_file<T: Write>(out: &mut T, func: &Function, il: &IlFunction, fileno: usize, ctx: &mut WriteContext) -> io::Result<()> {
    let mut last_span_id = IlSpanId::dummy();
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
        };

        if !i.node.is_label() {
            write!(out, "  ")?;
        };

        writeln!(out, "{}", i.pretty(&label_prefix))?;
    };

    writeln!(out, ".{}Lend_func:", label_prefix)?;
    writeln!(out, ".size {}, .{}Lend_func-{}", func_name, label_prefix, func_name)?;

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
