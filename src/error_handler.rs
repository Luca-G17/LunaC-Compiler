use colored::Colorize;

pub fn error(line_no: usize, message_str: String) {
    report(line_no, String::from(""), message_str);
}

pub fn report(line_no: usize, where_str: String, message_str: String) {
    println!("[line {}] Error {}: {}", line_no, where_str, message_str);
}

pub fn print_stage(content: String, header: String) {
    println!();
    println!("{}", header.bright_green());
    println!("{}", "---------------------------------------------------------".bright_yellow());
    print!("{}", content);
    println!();
}