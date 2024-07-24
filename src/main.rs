use std::env;
use compiler::translate_and_emulate;

mod compiler;
mod error_handler;
mod tests;
mod generated_tests;

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut filename = &String::from("test.c");
    if args.len() != 1 {
        filename = &args[1];
    }

    // translate_and_print(filename.to_string());
    translate_and_emulate(filename.to_string(), 1);
}

