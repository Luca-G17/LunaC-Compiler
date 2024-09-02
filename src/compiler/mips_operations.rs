use std::{collections::HashMap, fs};

use lazy_static::lazy_static;

use super::{scanner::{Token, TokenType}, translator::{translating_error, TranslatorError}};

const SPECIAL_FUNCTIONS_PATH: &str = "scripts/game_types.json";

#[derive(Clone)]
pub(super) struct RegisterMapping {
    pub(super) reg_no: usize,
    pub(super) var_type: VarType,
}

#[derive(Clone)]
pub(super) struct StackMapping {
    pub(super) relative_addr: usize,
    pub(super) var_type: VarType
}

#[derive(Clone)]
#[derive(PartialEq)]
pub(super) enum VarType {
    Int,
    Float,
}

impl VarType {
    pub(super) fn from_token(tok: &Token) -> Result<Self, TranslatorError> {
        if tok.tok_type == TokenType::Int {
            Ok(VarType::Int)
        }
        else if tok.tok_type == TokenType::Float {
            return Ok(VarType::Float)
        }
        else {
            translating_error(tok, String::from("Unrecognised argument type"));
            return Err(TranslatorError)
        }
    }

    pub(super) fn to_token(&self) -> Token {
        match self {
            VarType::Int => Token { tok_type: TokenType::Int, lexeme: String::from("int"), literal: String::from("int"), line_no: 0 },
            VarType::Float => Token { tok_type: TokenType::Float, lexeme: String::from("float"), literal: String::from("float"), line_no: 0 },
        }
    }
}

#[derive(Clone)]
pub(super) enum VariableMapping {
    RegisterMapping(RegisterMapping),
    StackMapping(StackMapping),
    StackPointer,
    ReturnAddress
}

impl VariableMapping {
    pub(super) fn from_register_number(reg_no: usize, var_type: VarType) -> Self {
        VariableMapping::RegisterMapping(RegisterMapping { reg_no, var_type })
    }

    pub(super) fn get_var_type(&self) -> VarType {
        match self {
            VariableMapping::RegisterMapping(reg) => reg.var_type.clone(),
            VariableMapping::StackMapping(sta) => sta.var_type.clone(),
            VariableMapping::StackPointer => VarType::Int,
            VariableMapping::ReturnAddress => VarType::Int,
        }
    }

    pub(super) fn get_address(&self) -> usize {
        match self {
            VariableMapping::RegisterMapping(reg_map) => reg_map.reg_no,
            VariableMapping::StackMapping(stc_map) => stc_map.relative_addr,
            VariableMapping::StackPointer => 0,
            VariableMapping::ReturnAddress => 1
        }
    }
}

// TODO: This has become quite messy im tempted to move the operation variance into a variable and collapse the variants into a single struct.
#[derive(Clone)]
pub(super) struct Move {
    pub(super) store: VariableMapping,
    pub(super) op_1: MipsOperand
}

#[derive(Clone)]
pub(super) struct Add {
    pub(super) store: VariableMapping,
    pub(super) op_1: MipsOperand,
    pub(super) op_2: MipsOperand
}

#[derive(Clone)]
pub(super) struct Sub {
    pub(super) store: VariableMapping,
    pub(super) op_1: MipsOperand,
    pub(super) op_2: MipsOperand
}

#[derive(Clone)]
pub(super) struct Mul {
    pub(super) store: VariableMapping,
    pub(super) op_1: MipsOperand,
    pub(super) op_2: MipsOperand
}


#[derive(Clone)]
pub(super) struct Mod {
    pub(super) store: VariableMapping,
    pub(super) op_1: MipsOperand,
    pub(super) op_2: MipsOperand
}

#[derive(Clone)]
pub(super) struct Div {
    pub(super) store: VariableMapping,
    pub(super) op_1: MipsOperand,
    pub(super) op_2: MipsOperand
}

#[derive(Clone)]
pub(super) struct Floor {
    pub(super) store: VariableMapping,
    pub(super) op_1: MipsOperand
}

#[derive(Clone)]
pub(super) struct Push {
    pub(super) op_1: MipsOperand,
}

#[derive(Clone)]
pub(super) struct Peek {
    pub(super) op_1: MipsOperand,
}

#[derive(Clone)]
pub(super) struct Label {
    pub(super) label_name: String
}

#[derive(Clone)]
pub(super) struct JumpAndSave {
    pub(super) label_name: String
}

#[derive(Clone)]
pub(super) struct And {
    pub(super) store: VariableMapping,
    pub(super) op_1: MipsOperand,
    pub(super) op_2: MipsOperand
}

#[derive(Clone)]
pub(super) struct Or {
    pub(super) store: VariableMapping,
    pub(super) op_1: MipsOperand,
    pub(super) op_2: MipsOperand
}

#[derive(Clone)]
pub(super) struct Not {
    pub(super) store: VariableMapping,
    pub(super) op_1: MipsOperand,
}

#[derive(Clone)]
pub(super) struct Xor {
    pub(super) store: VariableMapping,
    pub(super) op_1: MipsOperand,
    pub(super) op_2: MipsOperand
}

#[derive(Clone)]
pub(super) struct Seq {
    pub(super) store: VariableMapping,
    pub(super) op_1: MipsOperand,
    pub(super) op_2: MipsOperand
}

#[derive(Clone)]
pub(super) struct Sgt {
    pub(super) store: VariableMapping,
    pub(super) op_1: MipsOperand,
    pub(super) op_2: MipsOperand
}

#[derive(Clone)]
pub(super) struct Sge {
    pub(super) store: VariableMapping,
    pub(super) op_1: MipsOperand,
    pub(super) op_2: MipsOperand
}

#[derive(Clone)]
pub(super) struct Slt {
    pub(super) store: VariableMapping,
    pub(super) op_1: MipsOperand,
    pub(super) op_2: MipsOperand
}

#[derive(Clone)]
pub(super) struct Sle {
    pub(super) store: VariableMapping,
    pub(super) op_1: MipsOperand,
    pub(super) op_2: MipsOperand
}

#[derive(Clone)]
pub(super) struct Bne {
    pub(super) op_1: MipsOperand,
    pub(super) op_2: MipsOperand,
    pub(super) dest: MipsOperand
}

#[derive(Clone)]
pub(super) struct Beq {
    pub(super) op_1: MipsOperand,
    pub(super) op_2: MipsOperand,
    pub(super) dest: MipsOperand
}

#[derive(Clone)]
pub(super) struct StoringOperation {
    pub(super) store: VariableMapping,
    pub(super) operands: Vec<MipsOperand>,
    pub(super) op_str: String
}

#[derive(Clone)]
pub(super) struct NonStoringOperation {
    pub(super) operands: Vec<MipsOperand>,
    pub(super) op_str: String
}

#[derive(Clone)]
pub(super) struct Jump {
    pub(super) label_name: String
}

#[derive(Clone)]
pub(super) struct Return {}


#[derive(Clone)]
pub(super) enum MipsOperand {
    VariableMapping(VariableMapping),
    Literal(String)
}

pub struct SpecialFunction {
    args: Vec<String>,
    mips_name: String,
    storing: bool,
}

lazy_static! {
    static ref FUNCTIONS: HashMap<String, SpecialFunction> = {
        let mut m = HashMap::new();
        let functions_str = fs::read_to_string(SPECIAL_FUNCTIONS_PATH).expect("Failed to read special functions json file.");
        let functions_json = json::parse(&functions_str).unwrap();
        if let json::JsonValue::Array(function_arr) = &functions_json["special_functions"] {
            for function in function_arr {
                let identifier = match &function["identifier"] {
                    json::JsonValue::String(iden) => iden.to_string(),
                    _ => "".to_string()
                };

                let mips_name = match &function["mips_name"] {
                    json::JsonValue::String(name) => name.to_string(),
                    _ => "".to_string()
                };
                let mut args = vec![];
                if let json::JsonValue::Array(args_arr) = &function["args"] {
                    for arg in args_arr {
                        if let json::JsonValue::Array(arg_type_arr) = arg {
                            if let json::JsonValue::String(type_arg) = &arg_type_arr[0] {
                                args.push(type_arg.clone());
                            }
                        }
                    }
                }
                let storing = match &function["storing"] {
                    json::JsonValue::Boolean(storing_bool) => *storing_bool,
                    _ => false
                };
                
                m.insert(identifier, SpecialFunction { args, mips_name, storing });
            }
        }
        m
    };
}

impl MipsOperand {

    pub(super) fn from_unsigned_literal(integer: usize) -> Self {
        MipsOperand::Literal(format!("{}", integer))
    }

    pub(super) fn from_register_number(reg_no: usize, var_type: VarType) -> Self {
        MipsOperand::VariableMapping(VariableMapping::RegisterMapping(RegisterMapping { reg_no, var_type }))
    }

    pub(super) fn from_stack_addr(stack_addr: usize, var_type: VarType) -> Self {
        MipsOperand::VariableMapping(VariableMapping::StackMapping(StackMapping { relative_addr: stack_addr, var_type }))
    }

    pub(super) fn from_string_literal(str: String) -> Self {
        MipsOperand::Literal(str)
    }

    pub(super) fn from_number_literal(num: f32) -> Self {
        MipsOperand::Literal(format!("{}", num))
    }

    pub(super) fn get_var_type(&self) -> VarType {
        match self {
            MipsOperand::VariableMapping(var) => var.get_var_type(),
            MipsOperand::Literal(lit) => if lit.contains('.') { VarType::Float } else {VarType::Int },
        }
    }

    pub(super) fn combined_var_type(&self, other: &MipsOperand) -> VarType {
        let other_var_type = other.get_var_type();
        if other_var_type == VarType::Float || self.get_var_type() == VarType::Float {
            return VarType::Float;
        }
        VarType::Int
    }

    pub(super) fn implicit_conversion(&self, var_type: VarType) -> (MipsOperand, Vec<MipsOperation>) {
        let current = self.get_var_type();
        match self {
            MipsOperand::VariableMapping(var) => {
                match var {
                    VariableMapping::RegisterMapping(reg) => {
                        let new_op = MipsOperand::from_register_number(reg.reg_no, var_type.clone());
                        if var_type == VarType::Int && current != VarType::Int {
                            (new_op.clone(), Vec::from([MipsOperation::Floor(Floor { store: var.clone(), op_1: new_op.clone()})]))
                        }
                        else {
                            (new_op, Vec::new())
                        }
                        
                    },
                    VariableMapping::StackMapping(sta) => (MipsOperand::from_stack_addr(sta.relative_addr, var_type), Vec::new()),
                    _ => (self.clone(), Vec::new()),
                }
            },
            MipsOperand::Literal(lit) => { 
                if var_type == VarType::Int {
                    if let Some(prefix) = lit.split('.').next() {
                        (MipsOperand::from_string_literal(prefix.to_string()), Vec::new()) 
                    } else {
                        panic!()
                    }
                } else if !lit.contains('.') {
                    (MipsOperand::from_string_literal(lit.clone() + ".0"), Vec::new())
                }
                else {
                    (self.clone(), Vec::new())
                }
            },
        }
    }
}

#[derive(Clone)]
pub(super) enum MipsOperation {
    Move(Move),
    Add(Add),
    Sub(Sub),
    Mul(Mul),
    Div(Div),
    Floor(Floor),
    Mod(Mod),
    And(And),
    Or(Or),
    Not(Not),
    Xor(Xor),
    Beq(Beq),
    Bne(Bne),
    Seq(Seq),
    Sgt(Sgt),
    Sge(Sge),
    Slt(Slt),
    Sle(Sle),
    Push(Push),
    Peek(Peek),
    Label(Label),
    JumpAndSave(JumpAndSave),
    Jump(Jump),
    Return(Return),
    StoringOperation(StoringOperation),
    NonStoringOperation(NonStoringOperation)
}

impl MipsOperation {

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
            MipsOperand::VariableMapping(v) => Self::variable_mapping_to_string(v),
            MipsOperand::Literal(l) => l
        }
    }

    fn operands_to_string(operands: Vec<MipsOperand>) -> String {
        let mut str = String::from("");
        for operand in operands {
            str.push(' ');
            str.push_str(&Self::operand_to_string(operand));
        }
        str
    }

    fn storing_op_to_string(op_str: &str, store: VariableMapping, operands: Vec<MipsOperand>) -> String {
        let mut str = format!("{} ", op_str);
        let store_str = Self::variable_mapping_to_string(store);
        str.push_str(&store_str);
        str.push_str(&Self::operands_to_string(operands));
        str.push('\n');
        str
    }

    fn non_storing_op_to_string(op_str: &str, operands: Vec<MipsOperand>) -> String {
        let mut str = format!("{} ", op_str);
        str.push_str(&Self::operands_to_string(operands));
        str.push('\n');
        str
    }

    pub(super) fn mips_operation_to_string(op: MipsOperation) -> String {
        match op {
            MipsOperation::Add(o) => Self::storing_op_to_string("add", o.store, vec![o.op_1, o.op_2]),
            MipsOperation::Sub(o) => Self::storing_op_to_string("sub", o.store, vec![o.op_1, o.op_2]),
            MipsOperation::Mul(o) => Self::storing_op_to_string("mul", o.store, vec![o.op_1, o.op_2]),
            MipsOperation::Div(o) => Self::storing_op_to_string("div", o.store, vec![o.op_1, o.op_2]),
            MipsOperation::Mod(o) => Self::storing_op_to_string("mod", o.store, vec![o.op_1, o.op_2]),
            MipsOperation::Push(o) => Self::non_storing_op_to_string("push", vec![o.op_1]),
            MipsOperation::Peek(o) => Self::non_storing_op_to_string("peek", vec![o.op_1]),
            MipsOperation::Move(o) => Self::storing_op_to_string("move", o.store, vec![o.op_1]),
            MipsOperation::Not(o) => Self::storing_op_to_string("not", o.store, vec![o.op_1]),
            MipsOperation::Label(o) => format!("{}:\n", o.label_name),
            MipsOperation::JumpAndSave(o) => format!("jal {}\n", o.label_name),
            MipsOperation::Jump(o) => format!("j {}\n", o.label_name),
            MipsOperation::And(o) => Self::storing_op_to_string("and", o.store, vec![o.op_1, o.op_2]),
            MipsOperation::Or(o) => Self::storing_op_to_string("or", o.store, vec![o.op_1, o.op_2]),
            MipsOperation::Xor(o) => Self::storing_op_to_string("xor", o.store, vec![o.op_1, o.op_2]),
            MipsOperation::Bne(o) => Self::non_storing_op_to_string("bne", vec![o.op_1, o.op_2, o.dest]),
            MipsOperation::Beq(o) => Self::non_storing_op_to_string("beq", vec![o.op_1, o.op_2, o.dest]),
            MipsOperation::Seq(o) => Self::storing_op_to_string("seq", o.store, vec![o.op_1, o.op_2]),
            MipsOperation::Sgt(o) => Self::storing_op_to_string("sgt", o.store, vec![o.op_1, o.op_2]),
            MipsOperation::Sge(o) => Self::storing_op_to_string("sge", o.store, vec![o.op_1, o.op_2]),
            MipsOperation::Slt(o) => Self::storing_op_to_string("slt", o.store, vec![o.op_1, o.op_2]),
            MipsOperation::Sle(o) => Self::storing_op_to_string("sle", o.store, vec![o.op_1, o.op_2]),
            MipsOperation::Return(_) => String::from("j ra\n"),
            MipsOperation::Floor(o) => Self::storing_op_to_string("floor", o.store, vec![o.op_1]),
            MipsOperation::StoringOperation(o) => Self::storing_op_to_string(&o.op_str, o.store, o.operands),
            MipsOperation::NonStoringOperation(o) => Self::non_storing_op_to_string(&o.op_str, o.operands),
        }
    }

    pub(super) fn is_direct_replaced(op_str: &str) -> bool {
        FUNCTIONS.contains_key(op_str)
    }

    // Returns the corresponding mips operation + it's number of required arguments
    // If the function is attempting to store into a pointer - store the value in the base_ptr+1, preventing us from overwriting the address stored in base_ptr
    pub(super) fn direct_replaced_operation(op_str: &str, base_ptr: usize, operands: Vec<MipsOperand>, store_type: VarType) -> Option<(MipsOperation, usize, bool)>{
        FUNCTIONS.get(op_str).map(|func_template| ({
            if func_template.storing {
                let mut store_ptr = base_ptr;
                if !func_template.args.is_empty() && func_template.args[0] == "float*" { store_ptr += 1;}
                MipsOperation::StoringOperation(StoringOperation { op_str: func_template.mips_name.clone(), store: VariableMapping::from_register_number(store_ptr, store_type), operands })
            } else {
                MipsOperation::NonStoringOperation(NonStoringOperation { op_str: func_template.mips_name.clone(), operands })
            } 
        }, func_template.args.len(), !func_template.args.is_empty() && func_template.args[0] == "float*" ))
    }
}