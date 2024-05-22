use std::fs;

use crate::compiler::parser;
use crate::compiler::scanner;

#[cfg(test)]
pub mod test_runners {
    use super::*;

    #[test]
    pub fn function_parse() {
        test_parser(String::from("function_parse"), false);
    }

    #[test]
    pub fn while_parse() {
        test_parser(String::from("while_parse"), false);
    }

    #[test]
    pub fn for_parse() {
        test_parser(String::from("for_parse"), false);
    }

    #[test]
    pub fn if_parse() {
        test_parser(String::from("if_parse"), false)
    }
}

pub fn test_parser(test_name: String, verbose: bool) {
    let filename = format!("tests/{}.c", test_name);
    let contents = fs::read_to_string(filename).expect("Failed to read file");
    let tokens = scanner::scan_tokens(contents);
    let stmts = parser::parse(&tokens);
    let mut result = String::from("");
    for stmt in stmts {
        result.push_str(&parser::pretty_print_stmt(&stmt, 0));
    }

    let truth_filename = format!("tests/{}_truth", test_name);
    let truth_contents = fs::read_to_string(truth_filename).expect("Failed to read file");

    if verbose {
        println!("GOT: ");
        println!("{}", result);
        println!("EXPECTED: ");
        println!("{}", truth_contents);
    }
    assert_eq!(truth_contents, result);
}