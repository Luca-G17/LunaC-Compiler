use std::fs;
use mips_operations::MipsOperation;
use crate::error_handler::print_stage;

pub mod parser;
pub mod scanner;
pub mod translator;
pub mod mips_operations;
pub mod emulator;
pub mod library_constructs;

fn translate(filename: String, verbose: bool) -> (Vec<MipsOperation>, bool) {
    let contents = fs::read_to_string(filename).expect("Failed to read file");
    print_stage(contents.clone(), String::from("ORIGINAL CODE:"));
    
    let tokens = scanner::scan_tokens(contents, verbose);
    let stmts = parser::parse(&tokens, verbose);

    translator::translate_statements(stmts)
}

#[allow(dead_code)]
pub fn translate_and_print(filename: String, output_filename: String, verbose: bool) {
    let (ops, error_state) = translate(filename, verbose);
    if !error_state {
        let mips_string = translator::mips_operations_to_string(&ops); 
        print_stage(mips_string.clone(), String::from("TRANSLATION:"));
        write_to_file(output_filename, mips_string);
    }
}

pub fn write_to_file(filename: String, mips_string: String) {
    fs::write(filename, mips_string).expect("Failed to write to file");
}

pub fn translate_and_emulate(filename: String, ret_count: usize) -> Vec<f32> {
    let (ops, error_state) = translate(filename, true);
    if !error_state {
        print_stage(translator::mips_operations_to_string(&ops), String::from("TRANSLATION:"));
        emulator::process_operations(ops, 20, ret_count)
    } else {
        vec![]
    }
}