use std::fs;
use mips_operations::MipsOperation;
use crate::error_handler::print_stage;

pub mod parser;
pub mod scanner;
pub mod translator;
pub mod mips_operations;
pub mod emulator;

fn translate(filename: String) -> Vec<MipsOperation> {
    let contents = fs::read_to_string(filename).expect("Failed to read file");
    print_stage(contents.clone(), String::from("ORIGINAL CODE:"));
    
    let tokens = scanner::scan_tokens(contents);
    let stmts = parser::parse(&tokens);

    translator::translate_statements(stmts)
}

pub fn translate_and_print(filename: String) {
    let ops = translate(filename);
    print!("{}", translator::mips_operations_to_string(&ops));
}

pub fn translate_and_emulate(filename: String) -> f32 {
    let ops = translate(filename);
    print_stage(translator::mips_operations_to_string(&ops), String::from("TRANSLATION:"));
    emulator::process_operations(ops, 8)
}