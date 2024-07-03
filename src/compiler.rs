use std::fs;


pub mod parser;
pub mod scanner;
pub mod translator;
pub mod mips_operations;
pub mod emulator;


pub fn translate_and_emulate(filename: String) -> f32 {
    let contents = fs::read_to_string(filename).expect("Failed to read file");
    let tokens = scanner::scan_tokens(contents);
    let stmts = parser::parse(&tokens);
    let ops = translator::translate_statements(stmts);
    print!("{}", translator::mips_operations_to_string(&ops));
    emulator::process_operations(ops)
}