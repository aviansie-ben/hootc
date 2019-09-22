extern crate libhootc as lib;

use std::io::{self, Read};

use lib::analysis::error::PrettyError;
use lib::analysis::analyze_module;
use lib::lex::Lexer;
use lib::log::Log;
use lib::parse::parse_module;
use lib::types::{PrettyType, TypeId};

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() != 3 {
        eprintln!("Usage: {} <input file> <output file>", args.get(0).map_or("hootc", |s| s));
        std::process::exit(1);
    };

    let input_filename = std::path::Path::new(&args[1]).canonicalize().unwrap();
    let mut input_file = std::fs::File::open(&input_filename).unwrap();
    let mut contents = String::new();
    input_file.read_to_string(&mut contents).unwrap();

    let mut lexer = Lexer::new(&contents);
    let mut errors = vec![];

    let mut module = parse_module(&mut lexer).unwrap();

    analyze_module(&mut module, &mut errors);

    for e in errors.iter() {
        println!("{}", PrettyError(e, &module.types));
    };

    println!("## TYPE TABLE ##");
    for t in 0..(module.types.num_types()) {
        println!("  [{}]: {}", t, PrettyType(TypeId(t), &module.types));
    }
    println!();

    println!("## SYMBOL DEFINITION TABLE ##");
    for (sym_id, sym) in module.syms.all_symbols() {
        if let Some(fn_sym) = sym.fn_sym {
            print!("  [{}/{}]: ", fn_sym, sym_id);
        } else {
            print!("  [global/{}]: ", sym_id);
        };
        println!("{} {} {}", sym.name, sym.node, PrettyType(sym.ty, &module.types));
    }
    println!();

    if !errors.is_empty() {
        return;
    };

    let mut stdout = io::stdout();
    let mut log = Log(Some(&mut stdout));

    let mut program = lib::ilgen::generate_il(&module, &mut log);

    lib::optimizer::optimize_program(&mut program, &mut log);

    let mut out = std::fs::File::create(std::path::Path::new(&args[2])).unwrap();
    let mut ctx = lib::codegen::amd64::write::WriteContext::new(&module.syms);

    let fileno = ctx.add_file(input_filename.to_str().unwrap(), &mut out).unwrap();

    lib::codegen::amd64::write::write_start_to_file(&mut out, &mut ctx).unwrap();

    for (_, f) in program.funcs {
        let mut code = lib::codegen::amd64::gen::generate_code(&f, &mut log);
        lib::codegen::amd64::peephole::do_pre_ra_peephole(&mut code, &mut log);
        let mut code = lib::codegen::amd64::reg_alloc::RegisterAllocator::new(
            lib::codegen::amd64::calling_convention::SysVCallingConvention()
        ).allocate(code, &mut log);
        lib::codegen::amd64::peephole::do_post_ra_peephole(&mut code, &mut log);

        println!("\n");
        for instr in code.instrs.iter() {
            println!("{}", instr.pretty(""));
        };
        println!();

        lib::codegen::amd64::write::write_fn_to_file(&mut out, &code, &f, fileno, &mut ctx).unwrap();
    };

    lib::codegen::amd64::write::write_end_to_file(&mut out, &mut ctx).unwrap();
}
