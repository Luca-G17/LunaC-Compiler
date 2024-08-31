use colored::Colorize;

pub fn error(line_no: usize, message_str: String) {
    report(line_no, String::from(""), message_str);
}

pub fn report(line_no: usize, where_str: String, message_str: String) {
    let line = format!("[line {}] Error {}: {}", line_no, where_str, message_str);
    println!("{}", line.red());
}

#[allow(dead_code)]
pub fn warning(line_no: usize, where_str: String, message_str: String) {
    let line = format!("[line {}] Warning {}: {}", line_no, where_str, message_str);
    println!("{}", line.yellow());
}

pub fn general_warning(message_str: String) {
    let line = format!("Warning: {}", message_str);
    println!("{}", line.yellow());
}

pub fn print_stage(content: String, header: String) {
    println!();
    println!("{}", header.bright_green());
    println!("{}", "---------------------------------------------------------".bright_yellow());
    print!("{}", content);
    println!();
}