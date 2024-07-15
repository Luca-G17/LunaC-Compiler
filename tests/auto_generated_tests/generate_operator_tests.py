operators = [
    { 'op': '+', 'init': 0, 'operand': 1, 'result': '2.0', 'string_name': 'add' },
    { 'op': '-', 'init': 0, 'operand': 1, 'result': '-2.0', 'string_name': 'minus' },
    { 'op': '*', 'init': 1, 'operand': 2, 'result': '4.0', 'string_name': 'times' },
    { 'op': '/', 'init': 4, 'operand': 2, 'result': '1.0', 'string_name': 'divide' },
    { 'op': '|', 'init': 5, 'operand': 2, 'result': '7.0', 'string_name': 'or' },
    { 'op': '&', 'init': 3, 'operand': 1, 'result': '1.0', 'string_name': 'and' },
    { 'op': '^', 'init': 4, 'operand': 2, 'result': '4.0', 'string_name': 'xor' },
    { 'op': '%', 'init': 5, 'operand': 4, 'result': '1.0', 'string_name': 'mod' }
]

def indents(depth):
    return "    " * depth

def generate_test(operator, indent_depth=1):
    # generate rust test string
    # generate .c test file
    i = indents(indent_depth)
    test_name = f"{operator['string_name']}_auto"
    rust_str = f"{i}#[test]\n"
    rust_str += f"{i}pub fn {test_name}() {{\n"
    rust_str += f'{indents(indent_depth + 1)}test_translation(String::from("{test_name}"), String::from("auto_generated_tests"),{operator["result"]});\n'
    rust_str += f"{i}}}\n"

    c_test = f"int main() {{\n"
    c_test += f"{i}int x = {operator['init']};\n"
    c_test += f"{i}x {operator['op']}= {operator['operand']};\n"
    c_test += f"{i}x = x {operator['op']} {operator['operand']};\n"
    c_test += f"}}\n"

    with open(f'tests/auto_generated_tests/{test_name}.c', 'w') as cfile: 
        cfile.write(c_test)

    return rust_str

def generate_rust_tests(operators):
    rust_str = """#[allow(unused_imports)]\nuse crate::tests::test_translation;\n\n#[cfg(test)]\npub mod translator_tests {\n   use super::*;\n"""
    
    for op in operators:
        rust_str += generate_test(op)
    rust_str += "}\n"
    with open(f'src/generated_tests.rs', 'w') as rustfile:
        rustfile.write(rust_str)

generate_rust_tests(operators)