use std::collections::HashMap;
use std::{thread, time};
use crate::error_handler::print_stage;
use super::mips_operations::*;

const NUM_REGISTERS: usize = 16;
const STACK_SIZE: usize = 512;

fn operand_read(registers: & [f32], sp: &mut usize, ra: &mut usize, op: &MipsOperand) -> f32 {
    match op {
        MipsOperand::VariableMapping(var) => {
            match var {
                VariableMapping::RegisterMapping(reg_map) => registers[reg_map.reg_no],
                VariableMapping::StackMapping(_) => panic!("Stack mappings are temporary constructs they shouldn't reach the final binary"),
                VariableMapping::StackPointer => *sp as f32,
                VariableMapping::ReturnAddress => *ra as f32,
            }
        },
        MipsOperand::Literal(lit) => lit.parse().unwrap(),
    }
}

fn read_label_operand(op: &MipsOperand) -> String {
    match op {
        MipsOperand::Literal(lit) => lit.clone(),
        MipsOperand::VariableMapping(_) => panic!("branch label not found")
    }
}

fn mem_set(registers: &mut [f32], sp: &mut usize, ra: &mut usize, store: &VariableMapping, op: &MipsOperand) {
    let value = operand_read(registers, sp, ra, op);
    match store {
        VariableMapping::RegisterMapping(reg_map) => registers[reg_map.reg_no] = value,
        VariableMapping::StackMapping(_) => panic!("Stack mappings are temporary constructs they shouldn't reach the final binary"),
        VariableMapping::StackPointer => *sp = value as usize,
        VariableMapping::ReturnAddress => *ra = value as usize,
    }
}

fn process_operation(op: &MipsOperation, label_mapping: &HashMap<String, usize>, registers: &mut [f32], stack: &mut [f32], line_no: usize, sp: &mut usize, ra: &mut usize) -> usize {
    let mut line_no = line_no;
    match op {
        MipsOperation::Move(o) => mem_set(registers, sp, ra, &o.store, &o.op_1),
        MipsOperation::Add(o) => {
            let left = operand_read(registers, sp, ra, &o.op_1);
            let right = operand_read(registers, sp, ra, &o.op_2);
            mem_set(registers, sp, ra, &o.store, &MipsOperand::from_number_literal(left + right))
        },
        MipsOperation::Sub(o) => {
            let left = operand_read(registers, sp, ra, &o.op_1);
            let right = operand_read(registers, sp, ra, &o.op_2);
            mem_set(registers, sp, ra, &o.store, &MipsOperand::from_number_literal(left - right))
        },
        MipsOperation::Mul(o) => {
            let left = operand_read(registers, sp, ra, &o.op_1);
            let right = operand_read(registers, sp, ra, &o.op_2);
            mem_set(registers, sp, ra, &o.store, &MipsOperand::from_number_literal(left * right))
        },
        MipsOperation::Div(o) => {
            let left = operand_read(registers, sp, ra, &o.op_1);
            let right = operand_read(registers, sp, ra, &o.op_2);
            mem_set(registers, sp, ra, &o.store, &MipsOperand::from_number_literal(left / right))
        },
        MipsOperation::And(o) => {
            let left: i32 = operand_read(registers, sp, ra, &o.op_1) as i32;
            let right: i32 = operand_read(registers, sp, ra, &o.op_2) as i32;
            mem_set(registers, sp, ra, &o.store, &MipsOperand::from_number_literal((left & right) as f32))
        },
        MipsOperation::Or(o) => {
            let left: i32 = operand_read(registers, sp, ra, &o.op_1) as i32;
            let right: i32 = operand_read(registers, sp, ra, &o.op_2) as i32;
            mem_set(registers, sp, ra, &o.store, &MipsOperand::from_number_literal((left | right) as f32))
        },
        MipsOperation::Not(o) => {
            let left: i32 = operand_read(registers, sp, ra, &o.op_1) as i32;
            mem_set(registers, sp, ra, &o.store, &MipsOperand::from_number_literal((!left) as f32))
        },
        MipsOperation::Xor(o) => {
            let left: i32 = operand_read(registers, sp, ra, &o.op_1) as i32;
            let right: i32 = operand_read(registers, sp, ra, &o.op_2) as i32;
            mem_set(registers, sp, ra, &o.store, &MipsOperand::from_number_literal((left ^ right) as f32))
        },
        MipsOperation::Mod(o) => {
            let left: i32 = operand_read(registers, sp, ra, &o.op_1) as i32;
            let right: i32 = operand_read(registers, sp, ra, &o.op_2) as i32;
            mem_set(registers, sp, ra, &o.store, &MipsOperand::from_number_literal((left % right) as f32))    
        },

        MipsOperation::Beq(o) => {
            let left: i32 = operand_read(registers, sp, ra, &o.op_1) as i32;
            let right: i32 = operand_read(registers, sp, ra, &o.op_2) as i32;
            if left == right { 
                match label_mapping.get(&read_label_operand(&o.dest)) {
                    Some(val) => line_no = *val,
                    None => panic!("Label not found in mapping")
                }
            }
        },
        MipsOperation::Bne(o) => {
            let left: i32 = operand_read(registers, sp, ra, &o.op_1) as i32;
            let right: i32 = operand_read(registers, sp, ra, &o.op_2) as i32;
            if left != right { 
                match label_mapping.get(&read_label_operand(&o.dest)) {
                    Some(val) => line_no = *val,
                    None => panic!("Label not found in mapping")
                }
            }
        },
        MipsOperation::Seq(o) => {
            let left: i32 = operand_read(registers, sp, ra, &o.op_1) as i32;
            let right: i32 = operand_read(registers, sp, ra, &o.op_2) as i32;
            let result = if left == right { 1.0 } else { 0.0 };
            mem_set(registers, sp, ra, &o.store, &MipsOperand::from_number_literal(result));
        },
        MipsOperation::Sgt(o) => {
            let left: i32 = operand_read(registers, sp, ra, &o.op_1) as i32;
            let right: i32 = operand_read(registers, sp, ra, &o.op_2) as i32;
            let result = if left > right { 1.0 } else { 0.0 };
            mem_set(registers, sp, ra, &o.store, &MipsOperand::from_number_literal(result));
        }
        MipsOperation::Sge(o) => {
            let left: i32 = operand_read(registers, sp, ra, &o.op_1) as i32;
            let right: i32 = operand_read(registers, sp, ra, &o.op_2) as i32;
            let result = if left >= right { 1.0 } else { 0.0 };
            mem_set(registers, sp, ra, &o.store, &MipsOperand::from_number_literal(result));
        }
        MipsOperation::Slt(o) => {
            let left: i32 = operand_read(registers, sp, ra, &o.op_1) as i32;
            let right: i32 = operand_read(registers, sp, ra, &o.op_2) as i32;
            let result = if left < right { 1.0 } else { 0.0 };
            mem_set(registers, sp, ra, &o.store, &MipsOperand::from_number_literal(result));
        }
        MipsOperation::Sle(o) => {
            let left: i32 = operand_read(registers, sp, ra, &o.op_1) as i32;
            let right: i32 = operand_read(registers, sp, ra, &o.op_2) as i32;
            let result = if left <= right { 1.0 } else { 0.0 };
            mem_set(registers, sp, ra, &o.store, &MipsOperand::from_number_literal(result));
        }
        MipsOperation::Push(o) => {
            stack[*sp] = operand_read(registers, sp, ra, &o.op_1);
            *sp += 1;
        },
        MipsOperation::Peek(o) => {
            let value = stack[*sp - 1];
            match &o.op_1 {
                MipsOperand::VariableMapping(var) => mem_set(registers, sp, ra, var, &MipsOperand::from_number_literal(value)),
                MipsOperand::Literal(_) => panic!("Cannot peek into a literal store")
            }
        },
        MipsOperation::Label(_) => (),
        MipsOperation::JumpAndSave(o) => {
            // jal [label_name]
            *ra = line_no + 1;
            match label_mapping.get(&o.label_name) {
                Some(val) => line_no = *val,
                None => panic!("Label not found in mapping")
            }
        },
        MipsOperation::Jump(o) => {
            match label_mapping.get(&o.label_name) {
                Some(val) => line_no = *val,
                None => panic!("Label not found in mapping")      
            }
        },
        MipsOperation::Return(_) => {
            line_no = operand_read(registers, sp, ra, &MipsOperand::VariableMapping(VariableMapping::ReturnAddress)) as usize - 1;
        },
        MipsOperation::Floor(o) => {
            let left: f32 = operand_read(registers, sp, ra, &o.op_1);
            let result = left.floor();
            mem_set(registers, sp, ra, &o.store, &MipsOperand::from_number_literal(result));
        },
        MipsOperation::Sleep(o) => {
            let left  = operand_read(registers, sp, ra, &o.op_1) as u64;
            let seconds = time::Duration::from_secs(left);
            thread::sleep(seconds);
        }
        MipsOperation::Yield(_) => {
            thread::sleep(time::Duration::from_millis(500));
        }
        MipsOperation::Load(_) |
        MipsOperation::LoadReagent(_) |
        MipsOperation::LoadSlot(_) |
        MipsOperation::LoadBatch(_) |
        MipsOperation::LoadBatchSlot(_) |
        MipsOperation::LoadBatchWithName(_) |
        MipsOperation::LoadBatchWithNameSlot(_) |
        MipsOperation::Store(_) |
        MipsOperation::StoreSlot(_) |
        MipsOperation::StoreBatch(_) |
        MipsOperation::StoreBatchSlot(_) |
        MipsOperation::StoreBatchWithName(_) |
        MipsOperation::DirectReplaceUnaryOperation(_) |
        MipsOperation::DirectReplaceBinaryOperation(_) => {}
    }
    line_no
}

fn label_line_numbers(ops: &[MipsOperation]) -> HashMap<String, usize> {
    let mut label_mapping = HashMap::new();
    for (line_no, op) in ops.iter().enumerate() {
        if let MipsOperation::Label(label) = op {
            label_mapping.insert(label.label_name.clone(), line_no);
        }
    }
    label_mapping
}

fn print_stack(stack: &[f32], print_size: usize) {
    // Define the width of each box
    let box_width = 2;

    // Define box-drawing characters
    let top_left = '┌';
    let top_right = '┐';
    let bottom_left = '└';
    let bottom_right = '┘';
    let horizontal = '─';
    let vertical = '│';
    let top_t = '┬';
    let bottom_t = '┴';
    let intersection = '┼';
    let left_t = '├';
    let right_t = '┤';
    let mut stack_str = String::from("");


    // Compute paddngs
    let padding: Vec<usize> = (0..print_size).map(|i| i.to_string().len().max(stack[i].to_string().len()) + 2).collect();


    // Print top border
    stack_str.push_str(&format!("{}", top_left));
    stack_str.push_str(&horizontal.to_string().repeat(7).to_string());
    stack_str.push_str(&format!("{}", top_t));
    for (i, box_width) in padding.iter().enumerate().take(print_size) {
        stack_str.push_str(&horizontal.to_string().repeat(*box_width).to_string());
        if i < print_size - 1 {
            stack_str.push_str(&format!("{}", top_t));
        }
    }
    stack_str.push_str(&format!("{}\n", top_right));

    // Print middle part with numbers
    stack_str.push_str(&format!("{}", vertical));
    stack_str.push_str(" Index ");
    stack_str.push_str(&format!("{}", vertical));
    for i in 0..print_size {
        let num_str = i.to_string();
        let extra_padding = padding[i] - num_str.len() - 2;
        let padding = 1 + (box_width - num_str.len()) / 2;
        stack_str.push_str(&format!(
            "{}{}{}",
            " ".repeat(padding + extra_padding),
            num_str,
            " ".repeat(padding)
        ));
        stack_str.push_str(&format!("{}", vertical));
    }
    stack_str.push('\n');

    // Print middle border
    stack_str.push_str(&format!("{}", left_t));
    stack_str.push_str(&horizontal.to_string().repeat(7).to_string());
    stack_str.push_str(&format!("{}", intersection));
    for (i, box_width) in padding.iter().enumerate().take(print_size) {
        stack_str.push_str(&horizontal.to_string().repeat(*box_width).to_string());
        if i < print_size - 1 {
            stack_str.push_str(&format!("{}", intersection));
        }
    }
    stack_str.push_str(&format!("{}", right_t));

    stack_str.push('\n');

    // Print middle part with numbers
    stack_str.push_str(&format!("{}", vertical));
    stack_str.push_str(" Value ");
    stack_str.push_str(&format!("{}", vertical));
    for i in 0..print_size {
        let num_str = stack[i].to_string();
        let extra_padding = padding[i] - num_str.len() - 2;
        let padding = 1;
        stack_str.push_str(&format!(
            "{}{}{}",
            " ".repeat(padding + extra_padding),
            num_str,
            " ".repeat(padding)
        ));
        stack_str.push_str(&format!("{}", vertical));
    }
    stack_str.push('\n');

    // Print middle border
    stack_str.push_str(&format!("{}", bottom_left));
    stack_str.push_str(&horizontal.to_string().repeat(7).to_string());
    stack_str.push_str(&format!("{}", bottom_t));
    for (i, box_width) in padding.iter().enumerate().take(print_size) {
        stack_str.push_str(&horizontal.to_string().repeat(*box_width).to_string());
        if i < print_size - 1 {
            stack_str.push_str(&format!("{}", bottom_t));
        }
    }
    stack_str.push_str(&format!("{}\n", bottom_right));

    print_stage(stack_str, String::from("STACK STATE:"));
}

pub(super) fn process_operations(ops: Vec<MipsOperation>, stack_print_size: usize, ret_count: usize) -> Vec<f32> {
    let mut registers = [0.0; NUM_REGISTERS];
    let mut stack = [0.0; STACK_SIZE];
    let mut sp = 0;
    let mut ra = 0;
    let mut line_no = 0;
    let label_mapping = label_line_numbers(&ops);

    while line_no < ops.len() {
        let op = &ops[line_no];
        line_no = process_operation(op, &label_mapping, &mut registers, &mut stack, line_no, &mut sp, &mut ra);
        line_no += 1;
    }
    
    print_stack(&stack, stack_print_size);
    stack[1..(ret_count + 1)].to_vec()
}