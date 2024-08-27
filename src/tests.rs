use std::fs;
use std::process::Command;

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
    const TEST_DIRECTORY: &str = "translation_tests";

    #[test]
    pub fn fibonacci() {
        test_translation("fibonacci_recursive", TEST_DIRECTORY, 1);
    }

    #[test]
    pub fn sum_of_5s() {
        test_translation("sum_of_5s_&_9s", TEST_DIRECTORY, 1);
    }

    #[test]
    pub fn weird_loops() {
        test_translation("weird_loops", TEST_DIRECTORY, 1);
    }

    #[test]
    pub fn pointers_recursive() {
        test_translation("pointers_recursive", TEST_DIRECTORY, 1);
    }

    #[test]
    pub fn implicit_type_conversions() {
        test_translation("implicit_type_conversion", TEST_DIRECTORY, 3);
    }

    #[test]
    pub fn explicit_type_conversions() {
        test_translation("explicit_type_conversion", TEST_DIRECTORY, 1);
    }

    #[test]
    pub fn arrays() {
        test_translation("arrays", TEST_DIRECTORY, 4)
    }

}

#[allow(dead_code)]
pub fn test_translation(test_name: &str, test_dir: &str, result_count: usize) {
    let filename = format!("tests/{}/{}.c", test_dir, test_name);
    let result = translate_and_emulate(filename.clone(), result_count);

    let binary_string = format!("tests/{}/bin/{}", test_dir, test_name);
    let compiler_output = Command::new("tests/auto_build")
                                                                            .arg(filename.clone())
                                                                            .status();

    match compiler_output {
        Ok(e) => assert!(e.success()),
        Err(e) => assert!(false, "{}\n", e.to_string())
    }

    let execution_output = Command::new(format!("./{}", binary_string)).output();

    match execution_output {
        Ok(out) => {
            let output_string = String::from_utf8_lossy(&out.stdout);
            let output_lines = output_string.split("\n");
            for (res, truth_line) in result.iter().zip(output_lines) {
                let truth_value: f32 = truth_line.parse().unwrap();
                assert_eq!(*res, truth_value);
            }
        }
        Err(e) => assert!(false, "{}\n", e.to_string())
    }
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