use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Error;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use crate::error_handler::report;
use super::parser::{FuncCallStmt, FuncStmt, LiteralExpr, VarStmt, VariableExpr};
use super::{parser::{Stmt, Expr}, scanner::{Token, TokenType}};

const NUM_REGISTERS: usize = 16;
const STACK_BASE_REGISTER: usize = 0;
const RETURN_REGISTER: usize = 1;


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

impl<'a> Eq for FuncStmt<'a> {}

// These variants allow us to dynamic dispatch depending on the mapping type
#[derive(Clone)]
struct RegisterMapping {
    reg_no: usize
}

#[derive(Clone)]
struct StackMapping {
    relative_addr: usize,
}

#[derive(Clone)]
enum VariableMapping {
    RegisterMapping(RegisterMapping),
    StackMapping(StackMapping),
    StackPointer
}

#[derive(Clone)]
struct Move {
    store: VariableMapping,
    op_1: MipsOperand
}

#[derive(Clone)]
struct Add {
    store: VariableMapping,
    op_1: MipsOperand,
    op_2: MipsOperand
}

#[derive(Clone)]
struct Sub {
    store: VariableMapping,
    op_1: MipsOperand,
    op_2: MipsOperand
}

#[derive(Clone)]
struct Mul {
    store: VariableMapping,
    op_1: MipsOperand,
    op_2: MipsOperand
}

#[derive(Clone)]
struct Div {
    store: VariableMapping,
    op_1: MipsOperand,
    op_2: MipsOperand
}

#[derive(Clone)]
struct Push {
    op_1: MipsOperand,
}

#[derive(Clone)]
struct Peek {
    op_1: MipsOperand,
}

#[derive(Clone)]
struct Pop {
    op_1: MipsOperand,
}

#[derive(Clone)]
struct Label<> {
    label_name: String
}

#[derive(Clone)]
struct Jump<> {
    label_name: String
}

#[derive(Clone)]
struct JumpReg {
    reg: MipsOperand
}

#[derive(Clone)]
enum MipsOperand {
    VariableMapping(VariableMapping),
    Literal(String)
}

impl MipsOperand {
    fn from_register_number(reg_no: usize) -> Self {
        MipsOperand::VariableMapping(VariableMapping::RegisterMapping(RegisterMapping { reg_no }))
    }

    fn from_string_literal(str: String) -> Self {
        MipsOperand::Literal(str)
    }
}

#[derive(Clone)]
enum MipsOperation {
    Move(Move),
    Add(Add),
    Sub(Sub),
    Mul(Mul),
    Div(Div),
    Push(Push),
    Peek(Peek),
    Pop(Pop),
    Label(Label),
    Jump(Jump),
    JumpReg(JumpReg)
}

#[derive(Clone)]
struct Env<'a> {
    mapping: Box<HashMap<String, VariableMapping>>,
    functions: Box<HashMap<String, &'a FuncStmt<'a>>>,
    frame_ptr: usize,
    var_count: usize,
    frame_base_ptr: usize,
    reg_ptr: usize,
    parent: Option<Rc<RefCell<Env<'a>>>>,
    function_block: bool
}

impl<'a> Env<'a> {
    fn new(mapping: Box<HashMap<String, VariableMapping>>, functions: Box<HashMap<String, &'a FuncStmt<'a>>>, frame_ptr: usize, var_count: usize, frame_base_ptr: usize, parent: Option<Rc<RefCell<Env<'a>>>>, function_block: bool) -> Rc<RefCell<Env<'a>>> {
        return Rc::new(RefCell::new(Env {
            mapping,
            functions,
            frame_ptr,
            var_count,
            frame_base_ptr,
            parent,
            reg_ptr: 6,
            function_block
        }));
    }

    fn add_var(&mut self, var_name: &str) {
        if self.mapping.contains_key(var_name) {
            return;
        }

        self.mapping.insert(var_name.to_string(), VariableMapping::StackMapping(StackMapping { 
            relative_addr: self.frame_ptr,
        }));
        self.frame_ptr += 1;
        self.var_count += 1;
    }

    fn set_reg_ptr(&mut self, value: usize) {
        if let Some(parent) = &self.parent {
            parent.borrow_mut().set_reg_ptr(value);
        }
        else {
            self.reg_ptr = value;
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

    fn get_variable_index(&self, var_name: String) -> Option<usize> {
        let mapping = self.mapping.get(&var_name)?;
        match mapping {
            VariableMapping::RegisterMapping(reg_map) => Some(reg_map.reg_no),
            VariableMapping::StackMapping(stc_map) => Some(stc_map.relative_addr),
            VariableMapping::StackPointer => None
        }
    }

    fn get_variable_mapping(&self, var_name: &str) -> Option<VariableMapping> {
        if let Some(mapping) = self.mapping.get(var_name) {
            return Some(mapping.clone())
        }

        if let Some(p) = &self.parent {
            return p.borrow().get_variable_mapping(var_name);
        }

        None
    }

    fn add_function(&mut self, function: &'a FuncStmt<'a>) -> Result<(), Error> {
        if self.functions.contains_key(&function.name.lexeme) {
            translating_error(&function.name, format!("Found two definitions for function: {}", function.name.lexeme));
            return Err(Error);
        }
        self.functions.insert(function.name.lexeme.clone(), function);
        Ok(())
    }

    fn get_function<'b>(&'b self, func_call: &'b Token) -> Option<&'b FuncStmt> {
        if let Some(p) = &self.parent {
            let call = p.borrow().get_function(func_call);
            return call;
        }

        match self.functions.get(&func_call.lexeme) {
            Some(func_stmt) => return Some(*func_stmt),
            None => {
                translating_error(&func_call, format!("No definition for function found: {}", func_call.lexeme));
                return None;
            },
        }
    }
}

fn variable_mapping_to_string(var: VariableMapping, first: bool) -> (String, bool) {
    match var {
        VariableMapping::RegisterMapping(m) => (format!("r{}", m.reg_no), false),
        VariableMapping::StackMapping(m) => {
            todo!()
        }
        VariableMapping::StackPointer => (String::from("sp"), false)
    }
}

fn operand_to_string(oper: MipsOperand, first: bool) -> (String, bool) {
    match oper {
        MipsOperand::VariableMapping(v) => { variable_mapping_to_string(v, first) },
        MipsOperand::Literal(l) => (l, false)
    }
}

fn mips_binary_op_to_string(op_str: String, store: VariableMapping, op_1: MipsOperand, op_2: MipsOperand) -> String {
    let mut str = String::from("");

    let (store_str, _) = variable_mapping_to_string(store, true);
    let (mut op_1, stack) = operand_to_string(op_1, true);
    if stack {
        str.push_str(&op_1);
        op_1 = String::from("r14");
    }

    let (mut op_2, stack) = operand_to_string(op_2, false);
    if stack {
        str.push_str(&op_2);
        op_2 = String::from("r15");
    }
    str.push_str(&format!("{} {} {} {}\n", op_str, store_str, op_1, op_2));
    return str;
}

fn mips_unary_op_to_string(op_str: String, store: VariableMapping, op_1: MipsOperand) -> String {
    let mut str = String::from("");

    let (store_str, _) = variable_mapping_to_string(store, true);
    let (mut op_1, stack) = operand_to_string(op_1, true);
    if stack {
        str.push_str(&op_1);
        op_1 = String::from("r15");
    }

    str.push_str(&format!("{} {} {}\n", op_str, store_str, op_1));
    return str;
}

fn mips_unary_no_store_to_string(op_str: String, op_1: MipsOperand) -> String {
    let mut str = String::from("");
    let (mut op_1, stack) = operand_to_string(op_1, true);
    if stack {
        str.push_str(&op_1);
        op_1 = String::from("r15");
    }
    str.push_str(&format!("{} {}\n", op_str, op_1));
    return str;
}

fn mips_operation_to_string(op: MipsOperation) -> String {
    match op {
        MipsOperation::Add(o) => mips_binary_op_to_string(String::from("add"), o.store, o.op_1, o.op_2),
        MipsOperation::Sub(o) => mips_binary_op_to_string(String::from("sub"), o.store, o.op_1, o.op_2),
        MipsOperation::Mul(o) => mips_binary_op_to_string(String::from("mul"), o.store, o.op_1, o.op_2),
        MipsOperation::Div(o) => mips_binary_op_to_string(String::from("div"), o.store, o.op_1, o.op_2),
        MipsOperation::Push(o) => mips_unary_no_store_to_string(String::from("push"), o.op_1),
        MipsOperation::Peek(o) => mips_unary_no_store_to_string(String::from("peek"), o.op_1),
        MipsOperation::Pop(o) => mips_unary_no_store_to_string(String::from("pop"), o.op_1),
        MipsOperation::Move(o) => mips_unary_op_to_string(String::from("move"), o.store, o.op_1),
        MipsOperation::Label(o) => format!("{}:\n", o.label_name),
        MipsOperation::Jump(o) => format!("jal {}\n", o.label_name),
        MipsOperation::JumpReg(o) => mips_unary_no_store_to_string(String::from("j"), o.reg)
    }
}

fn mips_binary_operation(operator: &Token, var_ptr: usize, op_1: MipsOperand, op_2: MipsOperand) -> MipsOperation {
    match operator.tok_type {
        TokenType::Plus => MipsOperation::Add(Add {
            store: VariableMapping::RegisterMapping(RegisterMapping { reg_no: var_ptr }),
            op_1,
            op_2,
        }),
        TokenType::Minus => MipsOperation::Sub(Sub {
            store: VariableMapping::RegisterMapping(RegisterMapping { reg_no: var_ptr }),
            op_1,
            op_2,
        }),
        TokenType::Star => MipsOperation::Mul(Mul {
            store: VariableMapping::RegisterMapping(RegisterMapping { reg_no: var_ptr }),
            op_1,
            op_2,
        }),
        TokenType::Slash => MipsOperation::Div(Div {
            store: VariableMapping::RegisterMapping(RegisterMapping { reg_no: var_ptr }),
            op_1,
            op_2,
        }),
        _ => panic!("Internal compiler error - not a binary operation")
    }
}

fn mips_unary_operation(operator: &Token, var_ptr: usize, operand: MipsOperand) -> MipsOperation {
    match operator.tok_type {
        TokenType::Minus => MipsOperation::Mul(Mul {
            store: VariableMapping::RegisterMapping(RegisterMapping { reg_no: var_ptr }),
            op_1: operand,
            op_2: MipsOperand::Literal(String::from("-1"))
        }),
        _ => panic!("Internal compiler error - not a unary operation")
    }
}

/*
    Each intermediary binary/unary operation needs to be assigned to a memory location,
    var_ptr acts as an abstraction of these memory locations, var_ptr = 0 refers to
    variable being bound to the expression evaluation. Increasing values of var_ptr,
    represent memory locations irrespective of the registers/stack.

    A grouping operation will always require a tmp register and therefore a var_ptr
    increment.
*/
fn translate_ast<'a>(ast: &Expr<'a>, env: Rc<RefCell<Env<'a>>>, var_ptr: usize) -> Result<(Vec<MipsOperation>, usize), Error> {
    match ast {
        Expr::Binary(e) => {
            let mut ops = Vec::new();
            let mut var_ptr = var_ptr;
            let left_opt = get_atomic_operand(&e.left, env.clone(), var_ptr);
            let left_operand;
            let mut left_store_ptr = var_ptr;

            if let Some(left) = left_opt { 
                let stack_ops;
                (left_operand, stack_ops, var_ptr) = left?;
                ops.extend(stack_ops);
            }
            else {
                let left_translation;
                (left_translation, _) = translate_ast(&e.left, env.clone(), var_ptr)?;
                left_store_ptr = var_ptr;
                ops.extend(left_translation);
                left_operand = MipsOperand::VariableMapping(VariableMapping::RegisterMapping(RegisterMapping { reg_no: var_ptr }));
            }

            let right_opt = get_atomic_operand(&e.right, env.clone(), var_ptr);
            let right_operand;
            if let Some(right) = right_opt { 
                let stack_ops;
                (right_operand, stack_ops, var_ptr) = right?;
                ops.extend(stack_ops); 
            }
            else {
                let right_translation;
                (right_translation, _) = translate_ast(&e.right, env.clone(), var_ptr + 1)?;
                ops.extend(right_translation);
                right_operand = MipsOperand::VariableMapping(VariableMapping::RegisterMapping(RegisterMapping { reg_no: var_ptr + 1}));
            }

            ops.push(mips_binary_operation(e.operator, left_store_ptr, left_operand, right_operand));
            return Ok((ops, var_ptr));
        },
        Expr::Unary(e) => {
            let mut var_ptr = var_ptr;
            let mut ops = Vec::new();
            let oper_opt = get_atomic_operand(&e.right, env.clone(), var_ptr);
            let operand;
            if let Some(atomic_opt) = oper_opt { 
                let stack_ops;
                (operand, stack_ops, var_ptr) = atomic_opt?;
                ops.extend(stack_ops);
            }
            else {
                let translation;
                (translation, var_ptr) = translate_ast(&e.right, env.clone(), var_ptr)?;
                ops = translation;
                operand = MipsOperand::VariableMapping(VariableMapping::RegisterMapping(RegisterMapping { reg_no: var_ptr }));
            }

            ops.push(mips_unary_operation(e.operator, var_ptr, operand));
            return Ok((ops, var_ptr));
        },
        Expr::Literal(e)  => {
            let store = VariableMapping::RegisterMapping(RegisterMapping { reg_no: env.borrow().get_reg_ptr() });
            let val_str;
            match e.value {
                Some(v) => val_str = v.lexeme.clone(),
                None => return  Err(Error)
            }
            let value = MipsOperand::Literal(val_str);
            return Ok((Vec::from([
                MipsOperation::Move(Move {
                    store,
                    op_1: value  
                })
            ]), var_ptr));
        }
        Expr::Variable(var) => {
            let (_, ops, reg_ptr) = variable_expr_to_mips_access(var, env, var_ptr)?;
            return Ok((ops, reg_ptr))
        },
        Expr::Call(call) => {
            // Call inside of expression
            let env_copy = env.clone();
            let ops = translate_function_call(&call.call, env_copy)?;
            return Ok((ops, var_ptr))
        },
        Expr::Grouping(e) => translate_ast(&e.expression, env, var_ptr),
        Expr::Logical(_) => todo!(),
        Expr::Null(_) => todo!(),
    }
}

fn variable_expr_to_mips_access<'a>(var: &VariableExpr, env: Rc<RefCell<Env<'a>>>, reg_ptr: usize) -> Result<(MipsOperand, Vec<MipsOperation>, usize), Error> {
    let borrowed_env = env.borrow();
            
    // if var_mapping is a stack mapping we must append stack access instructions and return the appropriate temp register mapping at this point
    if let Some(mapping) = borrowed_env.get_variable_mapping(&var.name.lexeme) {
        match mapping {
            VariableMapping::RegisterMapping(r) => return Ok((MipsOperand::VariableMapping(VariableMapping::RegisterMapping(r)), Vec::new(), reg_ptr)),
            VariableMapping::StackMapping(s) => {
                let mut local_reg_ptr = reg_ptr;
                let mut ops = Vec::new();
                let tmp_store_reg = MipsOperand::from_register_number(local_reg_ptr);
                local_reg_ptr += 1;
                ops.push(MipsOperation::Add(Add { store: VariableMapping::StackPointer, op_1: MipsOperand::from_register_number(0), op_2: MipsOperand::Literal(format!("{}", s.relative_addr + 1)) }));
                ops.push(MipsOperation::Peek(Peek { op_1: tmp_store_reg.clone() }));
                return Ok((tmp_store_reg, ops, local_reg_ptr));
            },
            VariableMapping::StackPointer => todo!(),
        }
    }
    translating_error(var.name, String::from("Variable does not exist in scope."));
    return Err(Error);
}

// TODO: Result<Option<>> is extremely stinky - option determines wether the operand is atomic,
// result determines wether the atomic operand exists in the current environment
fn get_atomic_operand<'a>(expr: &Expr<'a>, env: Rc<RefCell<Env<'a>>>, reg_ptr: usize) -> Option<Result<(MipsOperand, Vec<MipsOperation>, usize), Error>> {
    match expr {
        Expr::Literal(lit) => {
            match lit.value {
                Some(v) => Some(Ok((MipsOperand::Literal(v.lexeme.clone()), Vec::new(), reg_ptr))),
                None => Some(Err(Error))
            }
        },
        Expr::Variable(var) => return Some(variable_expr_to_mips_access(var, env, reg_ptr)),
        _ => None
    }
}

fn write_to_stack(var_name: String, env: Rc<RefCell<Env>>, result_reg: usize) -> Vec<MipsOperation> {
    let stack_addr_opt = env.borrow().get_variable_index(var_name);
    if let Some(stack_addr) = stack_addr_opt {
        return Vec::from([
            MipsOperation::Add(Add { store: VariableMapping::StackPointer, op_1: MipsOperand::from_register_number(0), op_2: MipsOperand::from_string_literal(format!("{}", stack_addr + 1)) }),
            MipsOperation::Push(Push { op_1: MipsOperand::from_register_number(result_reg)})
        ])
    }
    panic!("Internal Compiler Error - Cannot find variable in the environment mappings")
}

fn translate_function_call<'a>(func_call: &FuncCallStmt<'a>, env: Rc<RefCell<Env<'a>>>) -> Result<Vec<MipsOperation>, Error> {
    // Copy parameter expressions to memory mappings
    let mut ops = Vec::new();
    
    let parent_env_opt = { env.borrow().parent.clone() };
    if let Some(parent) = parent_env_opt {
        let borrowed_parent = parent.borrow();
        let func_stmt_opt = { borrowed_parent.get_function(func_call.name) };
        if let Some(func_stmt) = func_stmt_opt {
            let mut param_reg_ptr = 3;
            for (i, expr) in func_call.params.iter().enumerate() {
                if let Some(Stmt::Variable(_)) = func_stmt.params.get(i) {
                    let (func_param_ops, _) = translate_ast(expr, env.clone(), param_reg_ptr)?;
                    ops.extend(func_param_ops);
                    param_reg_ptr += 1;
                }
                else {
                    translating_error(func_call.name, String::from("Provided arguments do not match those specified in the function signature"));
                    return Err(Error)
                }
            }
        }
        else {
            panic!("Internal Compiler Error - Function name not found in environment mapping");
        }
    }

    ops.push(MipsOperation::Add(Add { 
        store: VariableMapping::RegisterMapping(RegisterMapping { reg_no: STACK_BASE_REGISTER }),
        op_1: MipsOperand::from_register_number(STACK_BASE_REGISTER),
        op_2: MipsOperand::from_string_literal(format!("{}", env.borrow().frame_ptr))
    }));

    ops.push(MipsOperation::Jump(Jump { label_name: func_call.name.lexeme.clone() }));
    return Ok(ops);
}

fn translate<'a>(stmt: &Stmt<'a>, env: Rc<RefCell<Env<'a>>>) -> Result<Vec<MipsOperation>, Error> {
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
            ops.push(MipsOperation::Push(Push { op_1: MipsOperand::from_register_number(STACK_BASE_REGISTER) }));
            let func_env = Env::new(
                Box::new(HashMap::new()),
                Box::new(HashMap::new()),
                0,
                env.borrow().var_count,
                env.borrow().var_count,
                Some(Rc::clone(&env)),
                true
            );

            ops.push(MipsOperation::Move(Move {
                store: VariableMapping::RegisterMapping(RegisterMapping { reg_no: RETURN_REGISTER }),
                op_1: MipsOperand::Literal(String::from("ra"))
            }));

            let param_reg_ptr = 3;
            for (i, param) in func.params.iter().enumerate() {
                ops.push(MipsOperation::Push(Push { op_1: MipsOperand::from_register_number(param_reg_ptr + i) }));

                if let Stmt::Variable(p) = param {
                    func_env.borrow_mut().add_var(&p.var_name.lexeme.clone());
                }
            }

            // r0 stores the current base ptr
            // at the start of a function we must add the previous stack frame size to r0
            // on returns we must subtract this value
            let func_ops = translate(&func.body, func_env.clone())?;
            ops.extend(func_ops);
            return Ok(ops);
        },
        Stmt::If(_) => todo!(),
        Stmt::Return(ret) => {
            let mut ops = Vec::new();
            let reg_ptr = env.borrow().get_reg_ptr();
            if let Some(_) = &env.borrow().parent {
                let (ret_expr_ops, _) = translate_ast(&ret.value, env.clone(), reg_ptr)?;
                ops.extend(ret_expr_ops);
            }

            ops.push(MipsOperation::Pop(Pop { op_1: MipsOperand::from_register_number(STACK_BASE_REGISTER) }));
            ops.push(MipsOperation::JumpReg(JumpReg { reg: MipsOperand::from_register_number(RETURN_REGISTER)} ));
            return Ok(ops);
        },
        Stmt::Variable(var) => {
            let var_name = var.var_name.lexeme.clone();
            env.borrow_mut().add_var(&var_name);
            let mut var_ops = Vec::new();
            let reg_ptr = env.borrow().get_reg_ptr();
            match &var.initialiser {
                Some(init) => {
                    let (init_ops, _) = translate_ast(&init, env.clone(), reg_ptr)?;
                    var_ops.extend(init_ops);
                    var_ops.extend(write_to_stack(var_name.clone(), env.clone(), env.borrow().get_reg_ptr()));
                }
                None => {
                    todo!()
                }
            }
            return Ok(var_ops);
        },
        Stmt::While(_) => todo!(),
        Stmt::For(_) => todo!(),
        Stmt::Assign(assign) => {
            let return_register = env.borrow().get_reg_ptr();
            let (mut ops, _) = translate_ast(&assign.binding, env.clone(), return_register)?;
            ops.extend(write_to_stack(assign.var_name.lexeme.clone(), env.clone(), return_register));
            return Ok(ops)
        },
        Stmt::FunctionCall(func_call) => translate_function_call(func_call, env)
    }
}

fn translating_error(token: &Token, message: String) {
    if token.tok_type == TokenType::Eof {
        report(token.line_no, String::from(" at end"), message);
    }
    else {
        report(token.line_no, format!(" at '{}'", token.lexeme), message);
    }
}

pub fn translate_statements(statements: Vec<Stmt>) -> String {

    // Construct global environment
    let global_env = Env::new(
        Box::new(HashMap::new()), 
        Box::new(HashMap::new()), 
        0, 
        0,
        0,
        None,
        false
    );

    for stmt in &statements {
        if let Stmt::Function(s) = stmt {
            let _ = global_env.borrow_mut().add_function(s);
        }
        else if let Stmt::Variable(s) = stmt {
            global_env.borrow_mut().add_var(&s.var_name.lexeme.clone());
        }
    }
    
    let mut mips = Vec::new();
    for stmt in &statements {
        if let Ok(ops) = translate(&stmt, global_env.clone()) {
            mips = [mips, ops].concat();
        }
    }

    let mut ops_str = String::new();
    for op in mips {
        let op_string = mips_operation_to_string(op);
        ops_str.push_str(&op_string);
    }
    return ops_str;
}
