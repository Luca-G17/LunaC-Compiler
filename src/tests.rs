use std::fs;

use crate::compiler::parser;
use crate::compiler::scanner;
use crate::compiler::translate_and_emulate;

#[cfg(test)]
pub mod parser_tests {
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

#[cfg(test)]
pub mod translator_tests {
    use super::*;

    #[test]
    pub fn fibonacci() {
        test_translation(String::from("fibonacci_recursive"), 144.0)
    }

    #[test]
    pub fn sum_of_5s() {
        test_translation(String::from("sum_of_5s_&_9s"), 1509.0);
    }

    #[test]
    pub fn weird_loops() {
        test_translation(String::from("weird_loops"), -41.0);
    }
}

#[allow(dead_code)]
pub fn test_translation(test_name: String, expected: f32) {
    let filename = format!("tests/translation_tests/{}.c", test_name);
    let result = translate_and_emulate(filename);
    assert_eq!(result, expected);
}

#[allow(dead_code)]
pub fn test_parser(test_name: String, verbose: bool) {
    let filename = format!("tests/parser_tests/{}.c", test_name);
    let contents = fs::read_to_string(filename).expect("Failed to read file");
    let tokens = scanner::scan_tokens(contents);
    let stmts = parser::parse(&tokens);
    let mut result = String::from("");
    for stmt in stmts {
        result.push_str(&parser::pretty_print_stmt(&stmt, 0));
    }

    let truth_filename = format!("tests/parser_tests/{}_truth", test_name);
    let truth_contents = fs::read_to_string(truth_filename).expect("Failed to read file");

    if verbose {
        println!("GOT: ");
        println!("{}", result);
        println!("EXPECTED: ");
        println!("{}", truth_contents);
    }
    assert_eq!(truth_contents, result);
}