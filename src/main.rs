use clap::Parser;
use compiler::translate_and_print;

mod compiler;
mod error_handler;

mod tests;
mod generated_tests;

/// Translate a .c file into a stationeers MIPS assembly program.
#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    
    /// Input C file path
    input_file: String,

    /// Output MIPS asm file path
    #[arg(short, long)]
    output_file: Option<String>,

    #[arg(short, long)]
    verbose: bool
}


fn main() {
    let args = Args::parse();
    let filename = args.input_file;
    let output_file = match args.output_file {
        Some(str) => str,
        None => filename[0..(filename.len() - 2)].to_string()
    };


    translate_and_print(filename, output_file, args.verbose);
    // translate_and_emulate(filename, 1);
}

