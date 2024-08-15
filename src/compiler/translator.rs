use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use crate::error_handler::{general_warning, report};
use super::parser::{FuncCallStmt, FuncStmt, UnaryExpr, VariableExpr};
use super::{parser::{Stmt, Expr}, scanner::{Token, TokenType}};
use super::mips_operations::*;

const NUM_REGISTERS: usize = 16;
const STACK_BASE_REGISTER: usize = 0;


impl<'a> Hash for FuncStmt<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.lexeme.hash(state);
    }
}

impl<'a> PartialEq for FuncStmt<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.name.lexeme.eq(&other.name.lexeme)
    }
}
pub(super) struct TranslatorError;

#[derive(Clone)]
struct Variable {
    var_mapping: VariableMapping,
    size: Vec<usize>,
    is_array: bool,
    pub ptr_to_array: bool
}

impl Variable {
    fn step_size(&self, depth: usize, reading: bool) -> usize {
        return self.size[depth..].iter().fold(1, |acc, s| s * acc);
    } 
}

impl<'a> Eq for FuncStmt<'a> {}

#[derive(Clone)]
struct Env<'a> {
    mapping: Box<HashMap<String, Variable>>,
    functions: Rc<RefCell<HashMap<String, &'a FuncStmt<'a>>>>,
    frame_ptr: usize,
    var_count: usize,
    reg_ptr: usize,
    label_ptr: usize,
    parent: Option<Rc<RefCell<Env<'a>>>>,
}

impl<'a> Env<'a> {
    fn new(mapping: Box<HashMap<String, Variable>>, functions: Rc<RefCell<HashMap<String, &'a FuncStmt<'a>>>>, frame_ptr: usize, var_count: usize, parent: Option<Rc<RefCell<Env<'a>>>>) -> Rc<RefCell<Env<'a>>> {
        return Rc::new(RefCell::new(Env {
            mapping,
            functions,
            frame_ptr,
            var_count,
            parent,
            label_ptr: 0,
            reg_ptr: 6,
        }));
    }

    fn add_var(&mut self, var_name: &Token, var_type: VarType, var_size: Vec<usize>, is_array: bool, is_ptr_to_array: bool) {
        if self.mapping.contains_key(&var_name.lexeme.clone()) {
            return;
        }

        let var_mapping = VariableMapping::StackMapping(StackMapping { 
            relative_addr: self.frame_ptr,
            var_type
        });

        self.mapping.insert(var_name.lexeme.clone(), Variable { var_mapping, size: var_size, is_array, ptr_to_array: is_ptr_to_array }); 
        self.frame_ptr += 1;
        self.var_count += 1;
    }

    fn var_is_array_ptr(&self, var_name: &Token) -> Result<bool, TranslatorError> {
        let var = self.get_variable(var_name)?;
        Ok(var.is_array)
    }

    fn add_label(&mut self) -> usize {
        if let Some(parent) = &self.parent {
            return parent.borrow_mut().add_label();
        }
        else {
            self.label_ptr += 1;
            return self.label_ptr - 1;
        }
    }

    fn get_label_ptr(&self) -> usize {
        if let Some(parent) = &self.parent {
            return parent.borrow().get_label_ptr();
        }
        else {
            return self.label_ptr;
        }
    }

    fn get_reg_ptr(&self) -> usize {
        if let Some(parent) = &self.parent {
            return parent.borrow().get_reg_ptr();
        }
        else {
            return self.reg_ptr;
        }
    }

    fn get_variable_index(&self, var_name_tok: &Token) -> Result<usize, TranslatorError> {
        if let Some(mapping) = self.mapping.get(&var_name_tok.lexeme) {
            return match &mapping.var_mapping {
                VariableMapping::RegisterMapping(reg_map) => Ok(reg_map.reg_no),
                VariableMapping::StackMapping(stc_map) => Ok(stc_map.relative_addr),
                VariableMapping::StackPointer | VariableMapping::ReturnAddress => {
                    translating_error(var_name_tok, String::from("Variable does not exist."));
                    return Err(TranslatorError);
                },
            }
        }
        
        if let Some(p) = &self.parent {
            return p.borrow().get_variable_index(var_name_tok);
        }

        translating_error(var_name_tok, String::from("Variable does not exist in current scope"));
        return Err(TranslatorError);
    }

    fn get_variable(&self, var_name: &Token) -> Result<Variable, TranslatorError> {
        if let Some(var) = self.mapping.get(&var_name.lexeme) {
            return Ok(var.clone())
        }

        if let Some(p) = &self.parent {
            return p.borrow().get_variable(var_name);
        }

        translating_error(var_name, String::from("Variable does not exist in current scope."));
        Err(TranslatorError)
    }

    fn get_variable_mapping(&self, var_name: &Token) -> Result<VariableMapping, TranslatorError> {
        if let Some(mapping) = self.mapping.get(&var_name.lexeme) {
            return Ok(mapping.var_mapping.clone())
        }

        if let Some(p) = &self.parent {
            return p.borrow().get_variable_mapping(var_name);
        }

        translating_error(var_name, String::from("Variable does not exist in current scope."));
        Err(TranslatorError)
    }

    fn add_function(&mut self, function: &'a FuncStmt<'a>) -> Result<(), TranslatorError> {
        if self.functions.borrow().contains_key(&function.name.lexeme) {
            translating_error(&function.name, format!("Found two definitions for function: {}", function.name.lexeme));
            return Err(TranslatorError);
        }
        self.functions.borrow_mut().insert(function.name.lexeme.clone(), function);
        Ok(())
    }

    fn get_function<'b>(&'b self, func_call: &'b Token) -> Option<&'b FuncStmt> {
        match self.functions.borrow().get(&func_call.lexeme) {
            Some(func_stmt) => return Some(*func_stmt),
            None => {
                translating_error(&func_call, format!("No definition for function found: {}", func_call.lexeme));
                return None;
            },
        }
    }
}

fn variable_mapping_to_string(var: VariableMapping) -> String {
    match var {
        VariableMapping::RegisterMapping(m) => format!("r{}", m.reg_no),
        VariableMapping::StackMapping(_) => panic!("Internal Compiler Error - Stack mapping failed to be pre-translated"),
        VariableMapping::StackPointer => String::from("sp"),
        VariableMapping::ReturnAddress => String::from("ra")
    }
}

fn operand_to_string(oper: MipsOperand) -> String {
    match oper {
        MipsOperand::VariableMapping(v) => variable_mapping_to_string(v),
        MipsOperand::Literal(l) => l
    }
}

fn mips_binary_op_to_string(op_str: String, store: VariableMapping, op_1: MipsOperand, op_2: MipsOperand) -> String {
    let mut str = String::from("");
    let store_str= variable_mapping_to_string(store);
    let op_1 = operand_to_string(op_1);
    let op_2 = operand_to_string(op_2);
    str.push_str(&format!("{} {} {} {}\n", op_str, store_str, op_1, op_2));
    return str;
}

fn mips_unary_op_to_string(op_str: String, store: VariableMapping, op_1: MipsOperand) -> String {
    let mut str = String::from("");
    let store_str= variable_mapping_to_string(store);
    let op_1 = operand_to_string(op_1);
    str.push_str(&format!("{} {} {}\n", op_str, store_str, op_1));
    return str;
}

fn mips_unary_no_store_to_string(op_str: String, op_1: MipsOperand) -> String {
    let mut str = String::from("");
    let op_1 = operand_to_string(op_1);
    str.push_str(&format!("{} {}\n", op_str, op_1));
    return str;
}

fn mips_binary_branch_to_string(branch_op_str: String, op_1: MipsOperand, op_2: MipsOperand, dest: MipsOperand) -> String {
    let mut str = String::from("");
    let op_1 = operand_to_string(op_1);
    let op_2 = operand_to_string(op_2);
    let dest = operand_to_string(dest);
    str.push_str(&format!("{} {} {} {}\n", branch_op_str, op_1, op_2, dest));
    return str;
}

fn mips_operation_to_string(op: MipsOperation) -> String {
    match op {
        MipsOperation::Add(o) => mips_binary_op_to_string(String::from("add"), o.store, o.op_1, o.op_2),
        MipsOperation::Sub(o) => mips_binary_op_to_string(String::from("sub"), o.store, o.op_1, o.op_2),
        MipsOperation::Mul(o) => mips_binary_op_to_string(String::from("mul"), o.store, o.op_1, o.op_2),
        MipsOperation::Div(o) => mips_binary_op_to_string(String::from("div"), o.store, o.op_1, o.op_2),
        MipsOperation::Mod(o) => mips_binary_op_to_string(String::from("mod"), o.store, o.op_1, o.op_2),
        MipsOperation::Push(o) => mips_unary_no_store_to_string(String::from("push"), o.op_1),
        MipsOperation::Peek(o) => mips_unary_no_store_to_string(String::from("peek"), o.op_1),
        MipsOperation::Move(o) => mips_unary_op_to_string(String::from("move"), o.store, o.op_1),
        MipsOperation::Not(o) => mips_unary_op_to_string(String::from("not"), o.store, o.op_1),
        MipsOperation::Label(o) => format!("{}:\n", o.label_name),
        MipsOperation::JumpAndSave(o) => format!("jal {}\n", o.label_name),
        MipsOperation::Jump(o) => format!("j {}\n", o.label_name),
        MipsOperation::And(o) => mips_binary_op_to_string(String::from("and"), o.store, o.op_1, o.op_2),
        MipsOperation::Or(o) => mips_binary_op_to_string(String::from("or"), o.store, o.op_1, o.op_2),
        MipsOperation::Xor(o) => mips_binary_op_to_string(String::from("xor"), o.store, o.op_1, o.op_2),
        MipsOperation::Bne(o) => mips_binary_branch_to_string(String::from("bne"), o.op_1, o.op_2, o.dest),
        MipsOperation::Beq(o) => mips_binary_branch_to_string(String::from("beq"), o.op_1, o.op_2, o.dest),
        MipsOperation::Seq(o) => mips_binary_op_to_string(String::from("seq"), o.store, o.op_1, o.op_2),
        MipsOperation::Sgt(o) => mips_binary_op_to_string(String::from("sgt"), o.store, o.op_1, o.op_2),
        MipsOperation::Sge(o) => mips_binary_op_to_string(String::from("sge"), o.store, o.op_1, o.op_2),
        MipsOperation::Slt(o) => mips_binary_op_to_string(String::from("slt"), o.store, o.op_1, o.op_2),
        MipsOperation::Sle(o) => mips_binary_op_to_string(String::from("sle"), o.store, o.op_1, o.op_2),
        MipsOperation::Return(_) => String::from("j ra\n"),
        MipsOperation::Floor(o) => mips_unary_op_to_string(String::from("floor"), o.store, o.op_1),
    }
}

fn combine_binary_operand_types(operator: &Token, op_1: MipsOperand, op_2: MipsOperand) -> (MipsOperand, MipsOperand, VarType, Vec<MipsOperation>) {
    let mut ops = Vec::new();
    let ret_type;
    let combined_type;
    
    if operator.is_bitwise_operator() || operator.tok_type == TokenType::Percent {
        // both operands are implicitly converted to integers and store type is integer
        combined_type = VarType::Int;
        ret_type = VarType::Int;
    }
    else {
        combined_type = op_1.combined_var_type(&op_2);
        ret_type = if operator.is_float_returning_operator() { combined_type.clone() } else { VarType::Int };
    }

    let (new_op_1, conv_ops) = op_1.implicit_conversion(combined_type.clone());
    ops.extend(conv_ops);
    let (new_op_2, conv_ops) = op_2.implicit_conversion(combined_type);
    ops.extend(conv_ops);
    return (new_op_1, new_op_2, ret_type, ops);
}

fn mips_binary_operation(operator: &Token, var_ptr: usize, op_1: MipsOperand, op_2: MipsOperand) -> (Vec<MipsOperation>, VarType) {
    let mut ops = Vec::new();
    let (op_1, op_2, store_type, conv_ops) = combine_binary_operand_types(operator, op_1, op_2);
    let store = VariableMapping::from_register_number(var_ptr, store_type.clone());
    ops.extend(conv_ops);

    let op = match operator.tok_type {
        TokenType::Plus => MipsOperation::Add(Add { store, op_1, op_2 }),
        TokenType::Minus => MipsOperation::Sub(Sub { store, op_1, op_2 }),
        TokenType::Star => MipsOperation::Mul(Mul { store, op_1, op_2 }),
        TokenType::Slash => MipsOperation::Div(Div { store, op_1, op_2 }),
        TokenType::Percent => MipsOperation::Mod(Mod { store, op_1, op_2 }),
        TokenType::And | TokenType::BitwiseAnd => MipsOperation::And(And { store, op_1, op_2 }),
        TokenType::Or | TokenType::BitwiseOr => MipsOperation::Or(Or { store, op_1, op_2 }),
        TokenType::BitwiseXor => MipsOperation::Xor(Xor { store, op_1, op_2 }),
        TokenType::EqualEqual => MipsOperation::Seq(Seq { store, op_1, op_2 }),
        TokenType::Greater => MipsOperation::Sgt(Sgt { store, op_1, op_2 }),
        TokenType::GreaterEqual => MipsOperation::Sge(Sge { store, op_1, op_2 }),
        TokenType::Less => MipsOperation::Slt(Slt { store, op_1, op_2 }),
        TokenType::LessEqual => MipsOperation::Sle(Sle { store, op_1, op_2 }),
        _ => panic!("Internal compiler error - not a binary operation")
    };

    ops.push(op);
    return (ops, store_type);
}

fn unary_operand_types(operator: &Token, operand: MipsOperand) -> (MipsOperand, VarType, Vec<MipsOperation>) {
    if operator.tok_type == TokenType::Star || operator.tok_type == TokenType::Bang || operator.tok_type == TokenType::Tilde || operator.tok_type == TokenType::Int {
        let (new_op_1, conv_ops) = operand.implicit_conversion(VarType::Int);
        return (new_op_1, VarType::Int, conv_ops);
    } else if operator.tok_type == TokenType::Float {
        let (new_op_1, conv_ops) = operand.implicit_conversion(VarType::Float);
        return (new_op_1, VarType::Float, conv_ops)
    }

    let ret_type = operand.get_var_type();
    return (operand, ret_type, Vec::new());
}


fn mips_unary_operation(operator: &Token, var_ptr: usize, operand: &mut MipsOperand) -> (Vec<MipsOperation>, VarType) {
    let store_type;
    let mut ops;
    (*operand, store_type, ops) = unary_operand_types(operator, operand.clone()); 
    match operator.tok_type {
        TokenType::Minus => ops.push(MipsOperation::Mul(Mul {
            store: VariableMapping::from_register_number(var_ptr, store_type.clone()),
            op_1: operand.clone(),
            op_2: MipsOperand::Literal(String::from("-1"))
        })),
        TokenType::Bang | TokenType::Tilde => ops.push(MipsOperation::Not(Not { store: VariableMapping::from_register_number(var_ptr, store_type.clone()), op_1: operand.clone() })),
        TokenType::Star => {
            // Dereference read
            ops.push(MipsOperation::Move(Move { store: VariableMapping::StackPointer, op_1: operand.clone() }));
            ops.push(MipsOperation::Peek(Peek { op_1: operand.clone() })); // TODO: potentially change this to load into a new register0
        },
        TokenType::Int | TokenType::Float => {},
        TokenType::BitwiseAnd => (),
        _ => panic!("Internal compiler error - not a unary operation")
    }
    return (ops, store_type);
}

fn translate_ast<'a>(ast: &Expr<'a>, env: Rc<RefCell<Env<'a>>>, var_ptr: usize, stack_addr: usize, deref_depth: usize, reading: bool, indirect_write_operations: Option<Vec<MipsOperation>>) -> Result<(Vec<MipsOperation>, usize, VarType, Option<String>, Option<Token>), TranslatorError> {
    if var_ptr > NUM_REGISTERS {
        general_warning(format!("Translation uses > {} registers - Reduce function argument counts/array initialiser lengths.", NUM_REGISTERS))
    }
    match ast {
        Expr::Binary(e) => {
            let mut ops = Vec::new();
            let mut var_ptr = var_ptr;
            let mut left_operand;
            let mut left_store_ptr = var_ptr;
            
            let left_opt = get_atomic_operand(&e.left, env.clone(), var_ptr, false, reading, deref_depth);
            let left_lvalue;

            if let Some(left) = left_opt { 
                let stack_ops;
                (left_operand, stack_ops, var_ptr, left_lvalue) = left?;
                ops.extend(stack_ops);
            }
            else {
                let (left_translation, left_type, literal_value);
                (left_translation, _, left_type, literal_value, left_lvalue) = translate_ast(&e.left, env.clone(), var_ptr, stack_addr, deref_depth, reading, indirect_write_operations.clone())?;
                left_store_ptr = var_ptr;
                ops.extend(left_translation);
                
                left_operand = match literal_value {
                    Some(l) => MipsOperand::from_string_literal(l),
                    None => MipsOperand::from_register_number(var_ptr, left_type) 
                }
            }

            // If the operator is a logical or: 
            // after lhs translation jump if lhs == 1
            let mut dest_label = 0;
            if e.operator.tok_type == TokenType::Or {
                dest_label = env.borrow_mut().add_label();
                ops.push(MipsOperation::Beq(Beq { 
                    op_1: MipsOperand::from_register_number(var_ptr, VarType::Int), 
                    op_2: MipsOperand::from_string_literal(String::from("1")), 
                    dest: MipsOperand::from_string_literal(format!("%{}", dest_label - 1))
                }));
            }

            let right_lvalue;
            let right_opt = get_atomic_operand(&e.right, env.clone(), var_ptr, false, reading, deref_depth);
            let mut right_operand;
            if let Some(right) = right_opt { 
                let stack_ops;
                (right_operand, stack_ops, var_ptr, right_lvalue) = right?;
                ops.extend(stack_ops);
            }
            else {
                let (right_translation, right_type, literal_value);
                (right_translation, _, right_type, literal_value, right_lvalue) = translate_ast(&e.right, env.clone(), var_ptr + 1, stack_addr, deref_depth, reading, indirect_write_operations)?;
                ops.extend(right_translation);

                right_operand = match literal_value {
                    Some(l) => MipsOperand::from_string_literal(l),
                    None => MipsOperand::from_register_number(var_ptr + 1, right_type)
                }
            }

            let mut array_ptr_opt = None;
            if deref_depth > 0 {
                if let Some(left_lvalue_tok) = &left_lvalue { // Left resolves to an array ptr
                    let step_size = env.borrow().get_variable(&left_lvalue_tok)?.step_size(deref_depth, reading);
                    let new_right_store = VariableMapping::from_register_number(var_ptr + 1, VarType::Int);
                    if step_size > 0 {
                        ops.push(MipsOperation::Mul(Mul { store: new_right_store.clone(), op_1: right_operand, op_2: MipsOperand::from_unsigned_literal(step_size) }));
                        right_operand = MipsOperand::VariableMapping(new_right_store);
                    }
                    array_ptr_opt = left_lvalue; 
                } else if let Some(right_lvalue_tok) = &right_lvalue { // Right resolves to an array ptr
                    let step_size = env.borrow().get_variable(&right_lvalue_tok)?.step_size(deref_depth, reading);                let new_left_store = VariableMapping::from_register_number(var_ptr, VarType::Int);
                    if step_size > 0 {
                        ops.push(MipsOperation::Mul(Mul { store: new_left_store.clone(), op_1: left_operand, op_2: MipsOperand::from_unsigned_literal(step_size) }));
                        left_operand = MipsOperand::VariableMapping(new_left_store);
                    }
                    array_ptr_opt = right_lvalue;
                }
            }

            let (op_ops, store_type) = mips_binary_operation(&e.operator, left_store_ptr, left_operand, right_operand);
            ops.extend(op_ops);
            
            if e.operator.tok_type == TokenType::Or {
                ops.push(MipsOperation::Label(Label { label_name: format!("%{}", dest_label - 1) }));
            }
            return Ok((ops, var_ptr, store_type, None, array_ptr_opt));
        },
        Expr::Unary(e) => {
            let mut var_ptr = var_ptr;
            let mut ops = Vec::new();
            let mut operand;

            let referencing = e.operator.tok_type == TokenType::BitwiseAnd;
            let dereferencing = e.operator.tok_type == TokenType::Star;
            let oper_opt = get_atomic_operand(&e.right, env.clone(), var_ptr, referencing, reading, deref_depth);
            let returning_literal_opt;
            let array_ptr_opt; 

            if let Some(atomic_opt) = oper_opt { 
                let stack_ops;
                (operand, stack_ops, var_ptr, array_ptr_opt) = atomic_opt?;
                ops.extend(stack_ops);
            }
            else {
                let translation;
                let res_type;
                let mut deref_depth = deref_depth;
                if dereferencing { deref_depth += 1; }
                (translation, var_ptr, res_type, _, array_ptr_opt) = translate_ast(&e.right, env.clone(), var_ptr, stack_addr, deref_depth, reading, indirect_write_operations)?;
                ops = translation;
                operand = MipsOperand::VariableMapping(VariableMapping::RegisterMapping(RegisterMapping { reg_no: var_ptr, var_type: res_type }));
            }

            let skip_operation = {
                match array_ptr_opt {
                    Some(_) => !(dereferencing && reading && deref_depth == 0),
                    None => false
                }
            };

            let store_type;
            if !skip_operation {
                let op_ops;
                (op_ops, store_type) = mips_unary_operation(&e.operator, var_ptr, &mut operand); 
                returning_literal_opt = if let MipsOperand::Literal(l) = operand { Some(l.clone()) } else { None };
                ops.extend(op_ops);
            } else {
                store_type = VarType::Int;
                returning_literal_opt = None;
            }

            return Ok((ops, var_ptr, store_type, returning_literal_opt, array_ptr_opt));
        },
        Expr::Literal(e)  => {
            let val_str;
            match e.value {
                Some(v) => val_str = v.lexeme.clone(),
                None => val_str = String::from("1")
            }
            let value = MipsOperand::Literal(val_str.clone());
            let store_type = value.get_var_type();
            let store = VariableMapping::from_register_number(var_ptr, store_type.clone());
            return Ok((
                Vec::from([ MipsOperation::Move(Move { store, op_1: value })]),
                var_ptr,
                store_type,
                Some(val_str),
                None
            ));
        }
        Expr::Variable(var) => {
            let (operand, ops, reg_ptr, _) = variable_expr_to_mips_access(var, env, var_ptr, false, reading, deref_depth)?;
            return Ok((ops, reg_ptr, operand.get_var_type(), None, None))
        },
        Expr::Call(call) => {
            // Call inside of expression
            // e.g. int x = foo(x, y) + bar(y);
            // Push intermediate results onto the stack
            let mut ops = Vec::new();
            let reg_ptr = env.borrow().get_reg_ptr();
            for i in reg_ptr..var_ptr {
                let stack_addr = env.borrow().frame_ptr + (i - reg_ptr);
                ops.push(MipsOperation::Add(Add { store: VariableMapping::StackPointer, op_1: MipsOperand::from_register_number(0, VarType::Int), op_2: MipsOperand::from_string_literal(format!("{}", stack_addr + 1)) }));
                ops.push(MipsOperation::Push(Push { op_1: MipsOperand::from_register_number(i, VarType::Float)}));
            }
            let (call_ops, ret_type) = translate_function_call(&call.call, env.clone(), env.borrow().frame_ptr + (var_ptr - reg_ptr))?;
            ops.extend(call_ops);

            for i in reg_ptr..var_ptr {
                // Add one to the reg ptr because r6 contains the returned value
                let stack_addr = env.borrow().frame_ptr + (i - reg_ptr);
                ops.push(MipsOperation::Add(Add { store: VariableMapping::StackPointer, op_1: MipsOperand::from_register_number(0, VarType::Int), op_2: MipsOperand::from_string_literal(format!("{}", stack_addr + 2)) }));
                ops.push(MipsOperation::Peek(Peek { op_1: MipsOperand::from_register_number(i + 1, VarType::Float)}));
            }
            return Ok((ops, var_ptr, ret_type, None, None))
        },
        Expr::Grouping(e) => translate_ast(&e.expression, env.clone(), var_ptr, stack_addr, deref_depth, reading, indirect_write_operations),
        Expr::StoredValueExpr(sto) => { 
            let op_1 = MipsOperand::Literal(format!("{}", sto.value));
            let store_type = op_1.get_var_type();
            return Ok((
                Vec::from([MipsOperation::Move(Move {
                    store: VariableMapping::from_register_number(var_ptr, store_type.clone()),
                    op_1
                })]),
                var_ptr,
                store_type,
                None,
                None
            ))
        },
        Expr::ArrayIntialiserExpr(init) => {
            let mut ops = Vec::new();
            for (i, expr) in init.elements.iter().enumerate() {
                let (e_ops, _, rhs_type, _, _) = translate_ast(&expr, env.clone(), var_ptr, stack_addr + (init.step_size * i), deref_depth, reading, indirect_write_operations.clone())?;
                ops.extend(e_ops);
                if let Expr::ArrayIntialiserExpr(_) = **expr {}
                else {
                    let offset = init.step_size * i;
                    let lvalue_type = VarType::from_token(init.var_type)?;
                    if let Some(ref write_instructions) = indirect_write_operations {
                        ops.extend(write_instructions.clone());
                        ops.push(MipsOperation::Add(Add { store: VariableMapping::from_register_number(var_ptr + 1, VarType::Int), op_1: MipsOperand::from_register_number(var_ptr + 1, VarType::Int), op_2: MipsOperand::from_unsigned_literal(offset) }));
                        ops.extend(indirect_write_to_stack(var_ptr, lvalue_type, rhs_type))
                    }
                    else {
                        ops.extend(write_to_stack(stack_addr + offset, var_ptr, lvalue_type, rhs_type));
                        env.borrow_mut().frame_ptr += 1;
                    }
                    
                }
            }
            return Ok((ops, var_ptr, VarType::Int, None, None))
        },
    }
}

fn translate_ast_and_write_to_stack<'a>(ast: &Expr<'a>, env: Rc<RefCell<Env<'a>>>, var_ptr: usize, var_stack_addr: usize, var_type: VarType, indirect_write_operations: Option<Vec<MipsOperation>>, writing_to_arr: bool) -> Result<Vec<MipsOperation>, TranslatorError> {
    let (mut ops, _, rhs_type, _, _) = translate_ast(ast, env, var_ptr, var_stack_addr, 0, true, indirect_write_operations.clone())?;
    
    match ast {
        Expr::ArrayIntialiserExpr(_) => (),
        _ => { 
            match indirect_write_operations {
                Some(write_ops) => {
                    ops.extend(write_ops);
                    if !writing_to_arr { 
                        ops.extend(indirect_write_to_stack(var_ptr, var_type, rhs_type));
                    } 
                    else {
                        ops.extend(indirect_write_to_stack(var_ptr, var_type, rhs_type));
                        // ops.push(MipsOperation::Move(Move { store: VariableMapping::StackPointer, op_1: MipsOperand::from_register_number(var_ptr + 1, VarType::Int)}));
                        // ops.push(MipsOperation::Push(Push { op_1: MipsOperand::from_register_number(var_ptr, var_type)}));
                    }
                },
                None => ops.extend(write_to_stack(var_stack_addr, var_ptr, var_type, rhs_type)),
            }
        },
    }
    
    return Ok(ops);
}

// TODO - reading is poorly named
fn variable_expr_to_mips_access<'a>(var: &VariableExpr, env: Rc<RefCell<Env<'a>>>, reg_ptr: usize, access_reference: bool, reading: bool, arr_ref_def: usize) -> Result<(MipsOperand, Vec<MipsOperation>, usize, Option<Token>), TranslatorError> {
    let borrowed_env = env.borrow();
    
    // if var_mapping is a stack mapping we must append stack access instructions and return the appropriate temp register mapping at this point
    let mapping = borrowed_env.get_variable_mapping(&var.name)?;
    let arr_ptr_tok = if env.borrow().var_is_array_ptr(var.name)? { Some(var.name.clone()) } else { None };

    match mapping {
        VariableMapping::RegisterMapping(r) => {
            return Ok((MipsOperand::VariableMapping(VariableMapping::RegisterMapping(r)), Vec::new(), reg_ptr, arr_ptr_tok));
        },
        VariableMapping::StackMapping(s) => {
            let mut local_reg_ptr = reg_ptr;
            let mut ops = Vec::new();
            let tmp_store_reg = MipsOperand::from_register_number(local_reg_ptr, s.var_type.clone());
            let mut offset: usize = 2;


            if access_reference {
                ops.push(MipsOperation::Add(Add { store: VariableMapping::RegisterMapping(RegisterMapping { reg_no: local_reg_ptr, var_type: s.var_type}), op_1: MipsOperand::from_register_number(STACK_BASE_REGISTER, VarType::Int), op_2: MipsOperand::Literal(format!("{}", s.relative_addr + offset)) }));     
            }
            else if let Some(arr_tok) = &arr_ptr_tok {
                let var = borrowed_env.get_variable(&arr_tok)?;
                if !var.ptr_to_array {
                    ops.push(MipsOperation::Add(Add { store: VariableMapping::RegisterMapping(RegisterMapping { reg_no: local_reg_ptr, var_type: s.var_type}), op_1: MipsOperand::from_register_number(STACK_BASE_REGISTER, VarType::Int), op_2: MipsOperand::Literal(format!("{}", s.relative_addr + offset)) }));
                } else {
                    if reading && !var.ptr_to_array { offset = 2; }
                    else if !reading && var.ptr_to_array { offset = 2; }
                    else if reading && var.ptr_to_array { 
                        offset = 2;
                    }
                    ops.push(MipsOperation::Add(Add { store: VariableMapping::StackPointer, op_1: MipsOperand::from_register_number(STACK_BASE_REGISTER, VarType::Int), op_2: MipsOperand::Literal(format!("{}", s.relative_addr + offset)) }));
                    ops.push(MipsOperation::Peek(Peek { op_1: tmp_store_reg.clone() }));
                }
            }
            else {
                ops.push(MipsOperation::Add(Add { store: VariableMapping::StackPointer, op_1: MipsOperand::from_register_number(STACK_BASE_REGISTER, VarType::Int), op_2: MipsOperand::Literal(format!("{}", s.relative_addr + offset)) }));
                ops.push(MipsOperation::Peek(Peek { op_1: tmp_store_reg.clone() }));
            }

            local_reg_ptr += 1;
            
            return Ok((tmp_store_reg, ops, local_reg_ptr, arr_ptr_tok));
        },
        VariableMapping::StackPointer | VariableMapping::ReturnAddress => todo!(),
    }
}

// TODO: Result<Option<>> is extremely stinky - option determines wether the operand is atomic,
// result determines wether the atomic operand exists in the current environment
fn get_atomic_operand<'a>(expr: &Expr<'a>, env: Rc<RefCell<Env<'a>>>, reg_ptr: usize, access_reference: bool, reading: bool, arr_ref_depth: usize) -> Option<Result<(MipsOperand, Vec<MipsOperation>, usize, Option<Token>), TranslatorError>> {
    match expr {
        Expr::Literal(lit) => {
            match lit.value {
                Some(v) => Some(Ok((MipsOperand::Literal(v.lexeme.clone()), Vec::new(), reg_ptr, None))),
                None => Some(Err(TranslatorError))
            }
        },
        Expr::Variable(var) => return Some(variable_expr_to_mips_access(var, env, reg_ptr, access_reference, reading, arr_ref_depth)),
        Expr::ArrayIntialiserExpr(init) => {
            translating_error(init.brace_tok, String::from("Expected expression."));
            return Some(Err(TranslatorError))
        }
        _ => None
    }
}

fn write_to_variable(var_name_tok: &Token, env: Rc<RefCell<Env>>, result_reg: usize, lvalue_type: VarType, rhs_type: VarType) -> Result<Vec<MipsOperation>, TranslatorError> {
    let stack_addr = env.borrow().get_variable_index(var_name_tok)?;
    return Ok(write_to_stack(stack_addr, result_reg, lvalue_type, rhs_type));
}

fn write_to_stack(relative_addr: usize, result_reg: usize, lvalue_type: VarType, rhs_type: VarType) -> Vec<MipsOperation> {
    let (_, _, mut ops) = unary_operand_types(&lvalue_type.to_token(), MipsOperand::from_register_number(result_reg, rhs_type));    
    ops.push(MipsOperation::Add(Add { store: VariableMapping::StackPointer, op_1: MipsOperand::from_register_number(STACK_BASE_REGISTER, VarType::Int), op_2: MipsOperand::from_string_literal(format!("{}", relative_addr + 1)) }));
    ops.push(MipsOperation::Push(Push { op_1: MipsOperand::from_register_number(result_reg, VarType::Int) }));
    return ops;
}

fn indirect_write_to_stack(result_reg: usize, lvalue_type: VarType, rhs_type: VarType) -> Vec<MipsOperation> {
    let (_, _, mut ops) = unary_operand_types(&lvalue_type.to_token(), MipsOperand::from_register_number(result_reg, rhs_type));    
    ops.push(MipsOperation::Add(Add { store: VariableMapping::StackPointer, op_1: MipsOperand::from_register_number(result_reg + 1, VarType::Int), op_2: MipsOperand::from_number_literal(-1.0) }));
    ops.push(MipsOperation::Push(Push { op_1: MipsOperand::from_register_number(result_reg, lvalue_type)}));
    return ops;
}

fn translate_function_call<'a>(func_call: &FuncCallStmt<'a>, env: Rc<RefCell<Env<'a>>>, frame_size: usize) -> Result<(Vec<MipsOperation>, VarType), TranslatorError> {
    // Copy parameter expressions to memory mappings
    let mut ops = Vec::new();
    let mut ret_type = VarType::Int;
    let parent_env_opt = { env.borrow().parent.clone() };
    if let Some(parent) = parent_env_opt {
        let borrowed_parent = parent.borrow();
        let func_stmt_opt = { borrowed_parent.get_function(func_call.name) };
        if let Some(func_stmt) = func_stmt_opt {
            let mut param_reg_ptr = 3;
            
            let ret_tok = func_stmt.ret_type;
            if ret_tok.tok_type == TokenType::Float { ret_type = VarType::Float }
            else if ret_tok.tok_type == TokenType::Int { ret_type = VarType::Int }
            else { panic!("Internal Compiler Error - Unrecognised return type token") };

            for (i, mut expr) in func_call.params.iter().enumerate() {
                if let Some(Stmt::Variable(var)) = func_stmt.params.get(i) {
                    let new_expr;
                    if var.is_array {
                        new_expr = Expr::Unary(UnaryExpr { operator: Token::generate_token(TokenType::BitwiseAnd, 0), right: Box::new(expr.clone()) });
                        expr = &new_expr;
                    }
                    let (func_param_ops, _, param_type, _, _) = translate_ast(expr, env.clone(), param_reg_ptr, 0, 0, true, None)?;
                    ops.extend(func_param_ops);
                    let (_, _, implicit_cast_ops) = unary_operand_types(var.var_type, MipsOperand::from_register_number(param_reg_ptr, param_type));
                    ops.extend(implicit_cast_ops);
                    param_reg_ptr += 1;
                }
                else {
                    translating_error(func_call.name, String::from("Provided arguments do not match those specified in the function signature"));
                    return Err(TranslatorError)
                }
            }
        }
        else {
            panic!("Internal Compiler Error - Function name not found in environment mapping");
        }
    }

    // Add number of variables in the current stack frame to the base pointer
    ops.push(MipsOperation::Add(Add { 
        store: VariableMapping::from_register_number(STACK_BASE_REGISTER, VarType::Int),
        op_1: MipsOperand::from_register_number(STACK_BASE_REGISTER, VarType::Int),
        op_2: MipsOperand::from_string_literal(format!("{}", frame_size + 1))
    }));
    ops.push(MipsOperation::JumpAndSave(JumpAndSave { label_name: func_call.name.lexeme.clone() }));
    ops.push(MipsOperation::Add(Add {
        store: VariableMapping::from_register_number(STACK_BASE_REGISTER, VarType::Int),
        op_1: MipsOperand::from_register_number(STACK_BASE_REGISTER, VarType::Int),
        op_2: MipsOperand::from_string_literal(format!("-{}", frame_size + 1))
    }));
    return Ok((ops, ret_type));
}

fn translate<'a>(stmt: &'a Stmt<'a>, env: Rc<RefCell<Env<'a>>>) -> Result<Vec<MipsOperation>, TranslatorError> {
    match stmt {
        Stmt::Block(block) => {
            let mut ops = Vec::new();
            for inner_stmt in block.statements.iter() {
                let inner_ops = translate(&inner_stmt, env.clone())?;
                ops = [ops, inner_ops].concat();
            }
            return Ok(ops)
        },
        Stmt::Expression(_) => todo!(),
        Stmt::Function(func) => {
            let mut ops = Vec::new();
            ops.push(MipsOperation::Label(Label { label_name: func.name.lexeme.clone() }));
            let func_env = Env::new(
                Box::new(HashMap::new()),
                env.borrow().functions.clone(),
                0,
                env.borrow().var_count,
                Some(Rc::clone(&env)),
            );

            ops.push(MipsOperation::Move(Move {
                store: VariableMapping::StackPointer,
                op_1: MipsOperand::from_register_number(0, VarType::Int)
            }));
            ops.push(MipsOperation::Push(Push { op_1: MipsOperand::VariableMapping(VariableMapping::ReturnAddress) }));

            let param_reg_ptr = 3;
            for (i, param) in func.params.iter().enumerate() {
                if let Stmt::Variable(p) = param {
                    let var_type = VarType::from_token(p.var_type)?;
                    func_env.borrow_mut().add_var(&p.var_name, var_type.clone(), p.var_sizes.clone(), p.is_array, p.is_array);
                    ops.extend(write_to_variable(&p.var_name, func_env.clone(), param_reg_ptr + i, var_type.clone(), var_type)?);
                }
            }

            // r0 stores the current base ptr
            // at the start of a function we must add the previous stack frame size to r0
            // on returns we must subtract this value
            let func_ops = translate(&func.body, func_env.clone())?;
            ops.extend(func_ops);

            if !func.is_main {
                ops.push(MipsOperation::Move(Move { store: VariableMapping::from_register_number(env.borrow().get_reg_ptr(), VarType::Int), op_1: MipsOperand::Literal(String::from("1")) }));
                ops.push(MipsOperation::Add(Add { store: VariableMapping::StackPointer, op_1: MipsOperand::from_register_number(0, VarType::Int), op_2: MipsOperand::Literal(String::from("1"))}));
                ops.push(MipsOperation::Peek(Peek { op_1: MipsOperand::VariableMapping(VariableMapping::ReturnAddress)}));
                ops.push(MipsOperation::Return(Return {}));    
            }
            return Ok(ops);
        },
        Stmt::If(conditional) => {
            let mut ops = Vec::new();
            let reg_ptr = env.borrow().get_reg_ptr();
            
            let mut current_label_ptr = env.borrow().get_label_ptr();
            let num_conditions = conditional.conditions.len();
            let final_label_ptr = current_label_ptr + num_conditions - 1;
            let final_label_str = format!("%{}", final_label_ptr);


            for (i, (condition, branch)) in (conditional.conditions.iter().zip(conditional.branches.iter())).enumerate() {
                if i != 0 {
                    ops.push(MipsOperation::Label(Label { label_name: format!("%{}", current_label_ptr) }))
                }
                let (condition_ops, _, _, _, _) = translate_ast(condition, env.clone(), reg_ptr, 0, 0, true, None)?;
                ops.extend(condition_ops);
                
                // Branch if condition is not true 
                current_label_ptr = env.borrow_mut().add_label();
                let branch_label = format!("%{}", current_label_ptr);
                ops.push(MipsOperation::Bne(Bne { 
                    op_1: MipsOperand::from_register_number(reg_ptr, VarType::Int),
                    op_2: MipsOperand::from_string_literal(String::from("1")),
                    dest: MipsOperand::from_string_literal(branch_label)
                }));

                let branch_block_env = Env::new(
                    Box::new(HashMap::new()),
                    env.borrow().functions.clone(),
                    0,
                    env.borrow().var_count,
                    Some(Rc::clone(&env)),
                );
                let branch_ops = translate(branch, branch_block_env)?;
                ops.extend(branch_ops);
                if i != num_conditions - 1 {
                    ops.push(MipsOperation::Jump(Jump { label_name: final_label_str.clone() }))
                }
            }
            ops.push(MipsOperation::Label(Label { label_name: format!("%{}", current_label_ptr)}));
            return Ok(ops);
        },
        Stmt::Return(ret) => {
            let mut ops = Vec::new();
            let reg_ptr = env.borrow().get_reg_ptr();
            if let Some(_) = &env.borrow().parent {
                let (ret_expr_ops, _, ret_type, _, _) = translate_ast(&ret.value, env.clone(), reg_ptr, 0, 0, true, None)?;
                ops.extend(ret_expr_ops);
                let (_, _, implicit_cast_ops) = unary_operand_types(&ret.ret_type, MipsOperand::from_register_number(reg_ptr, ret_type));
                ops.extend(implicit_cast_ops);
            }
            
            ops.push(MipsOperation::Add(Add { store: VariableMapping::StackPointer, op_1: MipsOperand::from_register_number(0, VarType::Int), op_2: MipsOperand::Literal(String::from("1"))}));
            ops.push(MipsOperation::Peek(Peek { op_1: MipsOperand::VariableMapping(VariableMapping::ReturnAddress)}));
            ops.push(MipsOperation::Return(Return {}));
            return Ok(ops);
        },
        Stmt::Variable(var) => {
            let var_type = VarType::from_token(var.var_type)?;
            env.borrow_mut().add_var(&var.var_name, var_type.clone(), var.var_sizes.clone(), var.is_array, false);
            let mut var_ops = Vec::new();
            let reg_ptr = env.borrow().get_reg_ptr();
            match &var.initialiser {
                Some(init) => {
                    let stack_addr = env.borrow().get_variable_index(var.var_name)?;
                    var_ops.extend(translate_ast_and_write_to_stack(&init, env, reg_ptr, stack_addr, var_type, None, false)?);
                }
                None => {}
            }
            return Ok(var_ops);
        },
        Stmt::While(wh) => {
            // j y
            // x:
            // loop body
            // y:
            // condition
            // beq condition 1 x
            let body_env = Env::new(
                Box::new(HashMap::new()),
                env.borrow().functions.clone(),
                0,
                env.borrow().var_count,
                Some(Rc::clone(&env)),
            );
            let mut ops = Vec::new();
            let body_label = format!("%{}", env.borrow_mut().add_label());
            ops.push(MipsOperation::Label(Label { label_name: body_label.clone() }));
            ops.extend(translate(&wh.body, body_env)?);

            let condition_label = format!("%{}", env.borrow_mut().add_label());
            ops.push(MipsOperation::Label(Label { label_name: condition_label}));

            let reg_ptr = env.borrow().get_reg_ptr();
            let (cond_ops, _, _, _, _) = translate_ast(&wh.condition, env.clone(), reg_ptr, 0, 0, true, None)?;
            ops.extend(cond_ops);
            ops.push(MipsOperation::Beq(Beq { 
                op_1: MipsOperand::from_register_number(reg_ptr, VarType::Int),
                op_2: MipsOperand::from_string_literal(String::from("1")),
                dest: MipsOperand::from_string_literal(body_label.clone())
            }));

            return Ok(ops)
        },
        Stmt::For(for_stmt) => {
            let mut ops = Vec::new();
            if let Some(variant) = &for_stmt.variant {
                ops.extend(translate(variant, env.clone())?);
            }

            let body_env = Env::new(
                Box::new(HashMap::new()),
                env.borrow().functions.clone(),
                0,
                env.borrow().var_count,
                Some(Rc::clone(&env)),
            );

            let body_label = format!("%{}", env.borrow_mut().add_label());
            ops.push(MipsOperation::Label(Label { label_name: body_label.clone() }));
            ops.extend(translate(&for_stmt.body, body_env)?);
            if let Some(incrementer) = &for_stmt.incrementer {
                ops.extend(translate(incrementer, env.clone())?)
            }

            let condition_label = format!("%{}", env.borrow_mut().add_label());
            ops.push(MipsOperation::Label(Label {label_name: condition_label}));
            let reg_ptr = env.borrow().get_reg_ptr();
            let (cond_ops, _, _, _, _) = translate_ast(&for_stmt.condition, env.clone(), reg_ptr, 0, 0, true, None)?;
            ops.extend(cond_ops);
            ops.push(MipsOperation::Beq(Beq { 
                op_1: MipsOperand::from_register_number(reg_ptr, VarType::Int),
                op_2: MipsOperand::from_string_literal(String::from("1")),
                dest: MipsOperand::from_string_literal(body_label.clone())
            }));
            
            return Ok(ops)
        },
        Stmt::Assign(assign) => {
            let return_register = env.borrow().get_reg_ptr();
            let address_register = return_register + 1;
            let mut ops = Vec::new();

            let lvalue_mapping = env.borrow().get_variable_mapping(&assign.lvalue_var)?;
            let lvalue_type = lvalue_mapping.get_var_type(); 
            let lvalue_addr = lvalue_mapping.get_address();
            // Check for variable in scope inside the ast translation

            match assign.lvalue_expr.lvalue_is_derefed() {
                Some(lvalue_expr) => {
                    // translate lvalue expression and store right expression result in defrefed location
                    // register r(return_register + 1) contains the stack address to store the rhs result in.
                    let (store_ops, _, _, _, arr_tok) = translate_ast(lvalue_expr, env.clone(), address_register, 0, 1, false, None)?;
                    let writing_to_arr = arr_tok.is_some();
                    ops.extend(translate_ast_and_write_to_stack(&assign.binding, env.clone(), return_register, lvalue_addr, lvalue_type, Some(store_ops), writing_to_arr)?); 
                }
                None => {
                   // lvalue must be a variable 
                    ops.extend(translate_ast_and_write_to_stack(&assign.binding, env, return_register, lvalue_addr, lvalue_type, None, false)?);
                }
            }
            return Ok(ops)
        },
        Stmt::FunctionCall(func_call) => {
            let (ops, _) = translate_function_call(func_call, env.clone(), env.borrow().frame_ptr)?;
            return Ok(ops)
        }
    }
}

pub(super) fn translating_error(token: &Token, message: String) {
    if token.tok_type == TokenType::Eof {
        report(token.line_no, String::from(" at end"), message);
    }
    else {
        report(token.line_no, format!(" at '{}'", token.lexeme), message);
    }
}

pub(super) fn mips_operations_to_string(ops: &Vec<MipsOperation>) -> String {
    let mut ops_str = String::new();
    for op in ops {
        let op_string = mips_operation_to_string(op.clone());
        ops_str.push_str(&op_string);
    }
    return ops_str;
}

pub(super) fn translate_statements(statements: Vec<Stmt>) -> Vec<MipsOperation> {
    // Construct global environment
    let global_env = Env::new(
        Box::new(HashMap::new()), 
        Rc::new(HashMap::new().into()), 
        0, 
        0,
        None,
    );

    for stmt in &statements {
        if let Stmt::Function(s) = stmt {
            let _ = global_env.borrow_mut().add_function(s);
        }
        else if let Stmt::Variable(s) = stmt {
            if let Ok(var_type) = VarType::from_token(s.var_type) {
                global_env.borrow_mut().add_var(&s.var_name, var_type, s.var_sizes.clone(), s.is_array, false);
            }
        }
    }
    
    let mut mips = Vec::new();
    mips.push(MipsOperation::Jump(Jump { label_name: String::from("main") }));
    for stmt in &statements {
        if let Ok(ops) = translate(&stmt, global_env.clone()) {
            mips = [mips, ops].concat();
        }
    }
    return mips;
}
