use std::env;
use compiler::translate_and_emulate;

mod compiler;
mod error_handler;
mod tests;

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut filename = &String::from("conditional_testing.c");
    if args.len() != 1 {
        filename = &args[1];
    }

    // translate_and_print(filename.to_string());
    translate_and_emulate(filename.to_string());
}

