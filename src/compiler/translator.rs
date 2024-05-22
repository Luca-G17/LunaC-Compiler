use std::collections::{HashMap, HashSet};
use std::env::var;
use std::fmt::Error;
use std::hash::{Hash, Hasher};

use crate::error_handler::report;
use super::parser::{FuncStmt, LiteralExpr, VarStmt, VariableExpr};
use super::{parser::{Stmt, Expr}, scanner::{Token, TokenType}};

const NUM_REGISTERS: usize = 16;


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
    relative_addr: usize    
}

#[derive(Clone)]
enum VariableMapping {
    RegisterMapping(RegisterMapping),
    StackMapping(StackMapping)
}

#[derive(Clone)]
struct Move<'a> {
    store: VariableMapping,
    op_1: MipsOperand<'a>
}

#[derive(Clone)]
struct Add<'a> {
    store: VariableMapping,
    op_1: MipsOperand<'a>,
    op_2: MipsOperand<'a>
}

#[derive(Clone)]
struct Sub<'a> {
    store: VariableMapping,
    op_1: MipsOperand<'a>,
    op_2: MipsOperand<'a>
}

#[derive(Clone)]
struct Mul<'a> {
    store: VariableMapping,
    op_1: MipsOperand<'a>,
    op_2: MipsOperand<'a>
}

#[derive(Clone)]
struct Div<'a> {
    store: VariableMapping,
    op_1: MipsOperand<'a>,
    op_2: MipsOperand<'a>
}

#[derive(Clone)]
struct Push<'a> {
    op_1: MipsOperand<'a>,
}

#[derive(Clone)]
struct Peek<'a> {
    op_1: MipsOperand<'a>,
}

#[derive(Clone)]
struct Pop<'a> {
    op_1: MipsOperand<'a>,
}

#[derive(Clone)]
enum MipsOperand<'a> {
    VariableMapping(VariableMapping),
    Literal(LiteralExpr<'a>)
}

#[derive(Clone)]
enum MipsOperation<'a> {
    Move(Move<'a>),
    Add(Add<'a>),
    Sub(Sub<'a>),
    Mul(Mul<'a>),
    Div(Div<'a>),
    Push(Push<'a>),
    Peek(Peek<'a>),
    Pop(Pop<'a>)
}

struct Env<'a> {
    mapping: HashMap<String, VariableMapping>,
    functions: HashSet<FuncStmt<'a>>,
    frame_ptr: usize,
    frame_base_ptr: usize
}

impl<'a> Env<'a> {
    fn add_var(&mut self, variable: VarStmt<'a>) -> String {
        let var_count = self.mapping.len();
        if var_count < (NUM_REGISTERS - 1) {
            self.mapping.insert(variable.var_name.lexeme.clone(), VariableMapping::RegisterMapping(RegisterMapping {
                reg_no: var_count
            }));
        }
        else {
            self.mapping.insert(variable.var_name.lexeme.clone(), VariableMapping::StackMapping(StackMapping { 
                relative_addr: self.frame_ptr
            }));
            self.frame_ptr += 1;
        }

        return String::from("");
    }

    fn initialise_variable(&self, variable: VarStmt<'a>, mapping: VariableMapping) -> String {
        match mapping {
            VariableMapping::RegisterMapping(reg_mapping) => {
                let reg_str = format!("move r{}", reg_mapping.reg_no);
                let mut str = String::from("");
                if let Some(init) = variable.initialiser {
                    // str.push_str(&translate_ast(&init));
                    return format!("{}", str);
                }
                else {
                    return format!("{} 0", reg_str);
                }
            },
            VariableMapping::StackMapping(stack_mapping) => todo!(),
        }
    }

    fn add_function(&mut self, function: FuncStmt<'a>) {
        self.functions.insert(function);
    }
}

#[derive(PartialEq, Eq, Hash)]
enum SingletonTokenType {
    Zero,
    One,
    MinusOne
}

fn generate_singleton_tokens() -> HashMap<SingletonTokenType, Token> {
    return HashMap::from([
        (SingletonTokenType::Zero, Token { tok_type: TokenType::Number, lexeme: String::from("0"), literal: String::from("0"), line_no: 0 }),
        (SingletonTokenType::One, Token { tok_type: TokenType::Number, lexeme: String::from("1"), literal: String::from("1"), line_no: 0 }),
        (SingletonTokenType::MinusOne, Token { tok_type: TokenType::Number, lexeme: String::from("-1"), literal: String::from("-1"), line_no: 0 })
    ]);
}

fn operand_to_string(oper: MipsOperand) -> String {
    match oper {
        MipsOperand::VariableMapping(_) => todo!(),
        MipsOperand::Literal(l) => {
            match l.value {

            }
        },
    }
}

fn mips_binary_op_to_string(op_str: String, op_1: MipsOperand, op_2: MipsOperand) -> String {
    return String::from("");
}

fn mips_unary_op_to_string(op_str: String, op_1: MipsOperand) -> String {
    return String::from("");
}

fn mips_operation_to_string(op: MipsOperation) -> String {
    match op {
        MipsOperation::Add(o) => return mips_binary_op_to_string(String::from("add"), o.op_1, o.op_2),
        MipsOperation::Sub(o) => return mips_binary_op_to_string(String::from("add"), o.op_1, o.op_2),
        MipsOperation::Mul(o) => return mips_binary_op_to_string(String::from("add"), o.op_1, o.op_2),
        MipsOperation::Div(o) => return mips_binary_op_to_string(String::from("add"), o.op_1, o.op_2),
        MipsOperation::Push(o) => return mips_unary_op_to_string(String::from("push"), o.op_1),
        MipsOperation::Peek(o) => return mips_unary_op_to_string(String::from("push"), o.op_1),
        MipsOperation::Pop(o) => return mips_unary_op_to_string(String::from("push"), o.op_1),
        MipsOperation::Move(o) => return mips_unary_op_to_string(String::from("push"), o.op_1),
    }
}

fn mips_binary_operation<'a>(operator: &'a Token, var_ptr: usize, op_1: MipsOperand<'a>, op_2: MipsOperand<'a>) -> MipsOperation<'a> {
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

fn mips_unary_operation<'a>(operator: &'a Token, var_ptr: usize, operand: MipsOperand<'a>, singleton_tokens: &'a HashMap<SingletonTokenType, Token>) -> MipsOperation<'a> {
    let minus_one = singleton_tokens.get(&SingletonTokenType::MinusOne);
    match operator.tok_type {
        TokenType::Minus => MipsOperation::Mul(Mul {
            store: VariableMapping::RegisterMapping(RegisterMapping { reg_no: var_ptr }),
            op_1: operand,
            op_2: MipsOperand::Literal(LiteralExpr { value: minus_one })
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
fn translate_ast<'a>(ast: &'a Expr<'a>, env: &'a Env<'a>, var_ptr: usize, singleton_tokens: &'a HashMap<SingletonTokenType, Token>) -> Result<(Vec<MipsOperation<'a>>, usize), Error> {
    match ast {
        Expr::Binary(e) => {
            let mut var_ptr = var_ptr;

            let mut ops = Vec::new();
            let left_opt = get_atomic_operand(&e.left, env);
            let right_opt = get_atomic_operand(&e.right, env);

            let left_operand;
            let right_operand;
            if let Some(left) = left_opt { left_operand = left?; }
            else {
                let left_translation ;
                (left_translation, var_ptr) = translate_ast(&e.left, env, var_ptr, singleton_tokens)?;
                ops = [ops, left_translation].concat();
                left_operand = MipsOperand::VariableMapping(VariableMapping::RegisterMapping(RegisterMapping { reg_no: var_ptr }));
            }

            if let Some(right) = right_opt { right_operand = right?; }
            else {
                let right_translation;
                (right_translation, var_ptr) = translate_ast(&e.right, env, var_ptr, singleton_tokens)?;
                ops = [ops, right_translation].concat();
                right_operand = MipsOperand::VariableMapping(VariableMapping::RegisterMapping(RegisterMapping { reg_no: var_ptr }));
            }

            ops.push(mips_binary_operation(e.operator, var_ptr, left_operand, right_operand));
            return Ok((ops, var_ptr));
        },
        Expr::Unary(e) => {
            let mut var_ptr = var_ptr;
            let mut ops = Vec::new();
            let oper_opt = get_atomic_operand(&e.right, env);
            let operand;
            if let Some(atomic_opt) = oper_opt { operand = atomic_opt?; }
            else {
                let translation;
                (translation, var_ptr) = translate_ast(&e.right, env, var_ptr, singleton_tokens)?;
                ops = translation;
                operand = MipsOperand::VariableMapping(VariableMapping::RegisterMapping(RegisterMapping { reg_no: var_ptr }));
            }

            ops.push(mips_unary_operation(e.operator, var_ptr, operand, singleton_tokens));
            return Ok((ops, var_ptr));
        },
        Expr::Literal(_) => todo!(),
        Expr::Call(_) => todo!(),
        Expr::Grouping(_) => todo!(),
        Expr::Logical(_) => todo!(),
        Expr::Variable(_) => todo!(),
        Expr::Null(_) => todo!(),
    }
}


// TODO: Result<Option<>> is extremely stinky - option determines wether the operand is atomic,
// result determines wether the atomic operand exists in the current environment

// This method also has a bunch of cloning - It's pretty stinky all round but I can't be bothered to fix it right now...
fn get_atomic_operand<'a>(expr: &'a Expr<'a>, env: &'a Env) -> Option<Result<MipsOperand<'a>, Error>> {
    match expr {
        Expr::Literal(lit) => {
            return Some(Ok(MipsOperand::Literal(lit.clone())))
        },
        Expr::Variable(var) => {
            let var_mapping = env.mapping.get(&var.name.lexeme.clone());
            match var_mapping {
                Some(v) => return Some(Ok(MipsOperand::VariableMapping(v.clone()))),
                None => {
                    translating_error(var.name, String::from("Variable does not exist in scope."));
                    return Some(Err(Error));
                }
            }
        }
        _ => None
    }
} 

fn translate(stmt: &Stmt, env: Vec<VarStmt>) -> String {
    match stmt {
        Stmt::Block(s) => {

            let mut env: Vec<VarStmt> = Vec::new();
            let mut str = String::from("");
            return str;
        },
        Stmt::Expression(s) => todo!(),
        Stmt::Function(_) => todo!(),
        Stmt::If(_) => todo!(),
        Stmt::Return(_) => todo!(),
        Stmt::Variable(_) => todo!(),
        Stmt::While(_) => todo!(),
        Stmt::For(_) => todo!(),
        Stmt::Assign(_) => todo!(),
        Stmt::FunctionCall(_) => todo!() 
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

pub fn translate_statements(statements: Vec<Stmt>) {
    let singleton_tokens = generate_singleton_tokens();

    // Construct global environment
    let mut base_env = Env {
        mapping: HashMap::new(),
        functions: HashSet::new(),
        frame_ptr: 0,
        frame_base_ptr: 0
    };

    for stmt in statements {
        if let Stmt::Function(s) = stmt {
            base_env.add_function(s);
        }
        else if let Stmt::Variable(s) = stmt {
            base_env.add_var(s);
        }
    }
}