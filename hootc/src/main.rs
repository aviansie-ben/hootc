extern crate libhootc as lib;

use std::io::{self, Read};

use lib::analysis::error::PrettyError;
use lib::analysis::typecheck::analyze_module;
use lib::sym::SymId;
use lib::lex::Lexer;
use lib::log::Log;
use lib::parse::parse_module;
use lib::types::{PrettyType, TypeId};

fn main() {
    let stdin = io::stdin();
    let mut contents = String::new();
    stdin.lock().read_to_string(&mut contents).unwrap();

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
    for (SymId(i), sym) in module.syms.all_symbols() {
        if let Some(fn_id) = sym.fn_id {
            print!("  [{}/{}]: ", fn_id, i);
        } else {
            print!("  [global/{}]: ", i);
        };
        println!("{} {} {}", sym.name, sym.node, PrettyType(sym.ty, &module.types));
    }
    println!();

    let mut stdout = io::stdout();
    let mut log = Log(Some(&mut stdout));

    let mut program = lib::ilgen::generate_il(&module, &mut log);

    lib::optimizer::optimize_program(&mut program, &mut log);

    for (_, f) in program.funcs {
        let mut code = lib::codegen::amd64::gen::generate_code(&f, &mut log);
        lib::codegen::amd64::peephole::do_pre_ra_peephole(&mut code, &mut log);
        let code = lib::codegen::amd64::reg_alloc::RegisterAllocator::new(
            lib::codegen::amd64::calling_convention::SysVCallingConvention()
        ).allocate(code, &mut log);

        println!("\n");
        for instr in code.instrs.iter() {
            println!("{}", instr.pretty());
        };
        println!();
    };
}
