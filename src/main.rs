use std::env;
use std::fs;
use compiler::parser;
use compiler::scanner;
use compiler::translator;
use tests::test_parser;

mod compiler;
mod error_handler;
mod tests;


fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];
    let contents = fs::read_to_string(filename).expect("Failed to read file");
    let tokens = scanner::scan_tokens(contents);
    let stmts = parser::parse(&tokens);
    let test = parser::parse_expr(&tokens);
    if let Some(e) = test  {
        println!("{}", parser::ast_pretty_printer(e));
    }
    //translator::translate_statements(stmts);
}

