use core::fmt;

use super::{scanner::{Token, TokenType}, translator::{translating_error, TranslatorError}};

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
pub(super) enum DirectReplaceUnaryOperationT {
    Sin,
    Cos,
    Tan,
    ASin,
    ACos,
    ATan,
    Abs,
    Ceil,
    Floor,
    Trunc,
    Round,
    Exp,
    Log,
    Sqrt,
    Rand,
}

impl fmt::Display for DirectReplaceUnaryOperationT {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            DirectReplaceUnaryOperationT::Cos => String::from("cos"),
            DirectReplaceUnaryOperationT::Tan => String::from("tan"),
            DirectReplaceUnaryOperationT::Sin => String::from("sin"),
            DirectReplaceUnaryOperationT::ASin => String::from("asin"),
            DirectReplaceUnaryOperationT::ACos => String::from("acos"),
            DirectReplaceUnaryOperationT::ATan => String::from("atan"),
            DirectReplaceUnaryOperationT::Abs => String::from("abs"),
            DirectReplaceUnaryOperationT::Ceil => String::from("ceil"),
            DirectReplaceUnaryOperationT::Floor => String::from("floor"),
            DirectReplaceUnaryOperationT::Trunc => String::from("trunc"),
            DirectReplaceUnaryOperationT::Round => String::from("round"),
            DirectReplaceUnaryOperationT::Exp => String::from("exp"),
            DirectReplaceUnaryOperationT::Log => String::from("log"),
            DirectReplaceUnaryOperationT::Sqrt => String::from("sqrt"),
            DirectReplaceUnaryOperationT::Rand => String::from("rand")
        })
    }
}

#[derive(Clone)]
pub(super) struct DirectReplaceUnaryOperation {
    pub(super) func_type: DirectReplaceUnaryOperationT,
    pub(super) store: VariableMapping,
    pub(super) op_1: MipsOperand
}

impl DirectReplaceUnaryOperation {
    fn from_func_type(func_type: DirectReplaceUnaryOperationT, base_ptr: usize, store_type: VarType) -> (MipsOperation, usize) {
        let store = VariableMapping::from_register_number(base_ptr, store_type);
        let op_1 = MipsOperand::VariableMapping(VariableMapping::from_register_number(base_ptr + 1, VarType::Float));
        (MipsOperation::DirectReplaceUnaryOperation(DirectReplaceUnaryOperation { func_type, store, op_1 }), 2)
    }
}

#[derive(Clone)]
pub(super) enum DirectReplaceBinaryOperationT {
    Max,
    Min,
    Atan2
}

impl fmt::Display for DirectReplaceBinaryOperationT {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            DirectReplaceBinaryOperationT::Max => String::from("max"),
            DirectReplaceBinaryOperationT::Min => String::from("min"),
            DirectReplaceBinaryOperationT::Atan2 => String::from("atan2")
        })
    }
}

#[derive(Clone)]
pub(super) struct DirectReplaceBinaryOperation {
    pub(super) func_type: DirectReplaceBinaryOperationT,
    pub(super) store: VariableMapping,
    pub(super) op_1: MipsOperand,
    pub(super) op_2: MipsOperand
}

impl DirectReplaceBinaryOperation {
    fn from_func_type(func_type: DirectReplaceBinaryOperationT, base_ptr: usize, store_type: VarType) -> (MipsOperation, usize) {
        let store = VariableMapping::from_register_number(base_ptr, store_type);
        let op_1 = MipsOperand::VariableMapping(VariableMapping::from_register_number(base_ptr + 1, VarType::Float));
        let op_2 = MipsOperand::VariableMapping(VariableMapping::from_register_number(base_ptr + 2, VarType::Float));
        (MipsOperation::DirectReplaceBinaryOperation(DirectReplaceBinaryOperation { func_type, store, op_1, op_2 }), 3)
    }
}

#[derive(Clone)]
pub(super) struct Yield {}

#[derive(Clone)]
pub(super) struct Sleep {
    pub(super) op_1: MipsOperand
}

#[derive(Clone)]
pub(super) struct Load {
    pub(super) store: VariableMapping,
    pub(super) device_id: MipsOperand,
    pub(super) device_variable_type: MipsOperand
}

#[derive(Clone)]
pub(super) struct LoadReagent {
    pub(super) store: VariableMapping,
    pub(super) device_id: MipsOperand,
    pub(super) reagent_mode: MipsOperand,
    pub(super) reagent_hash: MipsOperand
}

#[derive(Clone)]
pub(super) struct LoadSlot {
    pub(super) store: VariableMapping,
    pub(super) device_id: MipsOperand,
    pub(super) slot_index: MipsOperand,
    pub(super) slot_variable_type: MipsOperand
}

#[derive(Clone)]
pub(super) struct LoadBatch {
    pub(super) store: VariableMapping,
    pub(super) device_type: MipsOperand,
    pub(super) device_variable_type: MipsOperand,
    pub(super) batch_mode: MipsOperand
}

#[derive(Clone)]
pub(super) struct LoadBatchSlot {
    pub(super) store: VariableMapping,
    pub(super) device_type: MipsOperand,
    pub(super) slot_index: MipsOperand,
    pub(super) slot_variable_type: MipsOperand,
    pub(super) batch_mode: MipsOperand
}

#[derive(Clone)]
pub(super) struct LoadBatchWithName {
    pub(super) store: VariableMapping,
    pub(super) device_type: MipsOperand,
    pub(super) device_name: MipsOperand,
    pub(super) device_variable_type: MipsOperand,
    pub(super) batch_mode: MipsOperand
}

#[derive(Clone)]
pub(super) struct LoadBatchWithNameSlot {
    pub(super) store: VariableMapping,
    pub(super) device_type: MipsOperand,
    pub(super) device_name: MipsOperand,
    pub(super) slot_index: MipsOperand,
    pub(super) slot_variable_type: MipsOperand,
    pub(super) batch_mode: MipsOperand
}

#[derive(Clone)]
pub(super) struct Store {
    pub(super) device_id: MipsOperand,
    pub(super) device_variable_type: MipsOperand,
    pub(super) source: MipsOperand   
}

#[derive(Clone)]
pub(super) struct StoreSlot {
    pub(super) device_id: MipsOperand,
    pub(super) slot_index: MipsOperand,
    pub(super) slot_variable_type: MipsOperand,
    pub(super) source: MipsOperand
}

#[derive(Clone)]
pub(super) struct StoreBatch {
    pub(super) device_type: MipsOperand,
    pub(super) device_variable_type: MipsOperand,
    pub(super) source: MipsOperand
}


#[derive(Clone)]
pub(super) struct StoreBatchWithName {
    pub(super) device_type: MipsOperand,
    pub(super) device_name: MipsOperand,
    pub(super) device_variable_type: MipsOperand,
    pub(super) source: MipsOperand
}

#[derive(Clone)]
pub(super) struct StoreBatchSlot {
    pub(super) device_type: MipsOperand,
    pub(super) slot_index: MipsOperand,
    pub(super) slot_variable_type: MipsOperand,
    pub(super) source: MipsOperand
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
    Sleep(Sleep),
    Yield(Yield),
    Load(Load),
    LoadReagent(LoadReagent),
    LoadSlot(LoadSlot),
    LoadBatch(LoadBatch),
    LoadBatchSlot(LoadBatchSlot),
    LoadBatchWithName(LoadBatchWithName),
    LoadBatchWithNameSlot(LoadBatchWithNameSlot),
    Store(Store),
    StoreSlot(StoreSlot),
    StoreBatch(StoreBatch),
    StoreBatchSlot(StoreBatchSlot),
    StoreBatchWithName(StoreBatchWithName),
    DirectReplaceUnaryOperation
(DirectReplaceUnaryOperation
),
    DirectReplaceBinaryOperation(DirectReplaceBinaryOperation)
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
            MipsOperation::Sleep(o) => Self::non_storing_op_to_string("sleep", vec![o.op_1]),
            MipsOperation::Yield(_) => String::from("yield\n"),
            MipsOperation::Load(o) => Self::storing_op_to_string("l", o.store, vec![o.device_id, o.device_variable_type]),
            MipsOperation::LoadReagent(o) => Self::storing_op_to_string("lr", o.store, vec![o.device_id, o.reagent_mode, o.reagent_hash]),
            MipsOperation::LoadSlot(o) => Self::storing_op_to_string("ls", o.store, vec![o.device_id, o.slot_index, o.slot_variable_type]),
            MipsOperation::LoadBatchSlot(o) => Self::storing_op_to_string("lbs", o.store, vec![o.device_type, o.slot_index, o.slot_variable_type, o.batch_mode]),
            MipsOperation::LoadBatch(o) => Self::storing_op_to_string("lb", o.store, vec![o.device_type, o.device_variable_type, o.batch_mode]),
            MipsOperation::LoadBatchWithName(o) => Self::storing_op_to_string("lbn", o.store, vec![o.device_type, o.device_name, o.device_variable_type, o.batch_mode]),
            MipsOperation::LoadBatchWithNameSlot(o) => Self::storing_op_to_string("lbns", o.store, vec![o.device_type, o.device_name, o.slot_index, o.slot_variable_type, o.batch_mode]),
            MipsOperation::Store(o) => Self::non_storing_op_to_string("s", vec![o.device_id, o.device_variable_type, o.source]),
            MipsOperation::StoreSlot(o) => Self::non_storing_op_to_string("ss", vec![o.device_id, o.slot_index, o.slot_variable_type, o.source]),
            MipsOperation::StoreBatch(o) => Self::non_storing_op_to_string("sb", vec![o.device_type, o.device_variable_type, o.source]),
            MipsOperation::StoreBatchSlot(o) => Self::non_storing_op_to_string("sbs", vec![o.device_type, o.slot_index, o.slot_variable_type, o.source]),
            MipsOperation::StoreBatchWithName(o) => Self::non_storing_op_to_string("sbn", vec![o.device_type, o.device_name, o.device_variable_type, o.source]),
            MipsOperation::DirectReplaceUnaryOperation(o) => Self::storing_op_to_string(&o.func_type.to_string(), o.store, vec![o.op_1]),
            MipsOperation::DirectReplaceBinaryOperation(o) => Self::storing_op_to_string(&o.func_type.to_string(), o.store, vec![o.op_1, o.op_2]),
        }
    }

    pub(super) fn direct_replaced_operation(op_str: &str, base_ptr: usize, store_type: VarType) -> Option<(MipsOperation, usize)> {
        let op = match op_str {
            "m_sin" => DirectReplaceUnaryOperation::from_func_type(DirectReplaceUnaryOperationT::Sin, base_ptr, store_type),
            "m_cos" => DirectReplaceUnaryOperation::from_func_type(DirectReplaceUnaryOperationT::Cos, base_ptr, store_type),
            "m_tan" => DirectReplaceUnaryOperation::from_func_type(DirectReplaceUnaryOperationT::Tan, base_ptr, store_type),
            "m_acos" => DirectReplaceUnaryOperation::from_func_type(DirectReplaceUnaryOperationT::ACos, base_ptr, store_type),
            "m_asin" => DirectReplaceUnaryOperation::from_func_type(DirectReplaceUnaryOperationT::ASin, base_ptr, store_type),
            "m_atan" => DirectReplaceUnaryOperation::from_func_type(DirectReplaceUnaryOperationT::ATan, base_ptr, store_type),
            "m_abs" => DirectReplaceUnaryOperation::from_func_type(DirectReplaceUnaryOperationT::Abs, base_ptr, store_type),
            "m_ceil" => DirectReplaceUnaryOperation::from_func_type(DirectReplaceUnaryOperationT::Ceil, base_ptr, store_type),
            "m_floor" => DirectReplaceUnaryOperation::from_func_type(DirectReplaceUnaryOperationT::Floor, base_ptr, store_type),
            "m_trunc" => DirectReplaceUnaryOperation::from_func_type(DirectReplaceUnaryOperationT::Trunc, base_ptr, store_type),
            "m_round" => DirectReplaceUnaryOperation::from_func_type(DirectReplaceUnaryOperationT::Round, base_ptr, store_type),
            "m_exp" => DirectReplaceUnaryOperation::from_func_type(DirectReplaceUnaryOperationT::Exp, base_ptr, store_type),
            "m_log" => DirectReplaceUnaryOperation::from_func_type(DirectReplaceUnaryOperationT::Log, base_ptr, store_type),
            "m_sqrt" => DirectReplaceUnaryOperation::from_func_type(DirectReplaceUnaryOperationT::Sqrt, base_ptr, store_type),
            "m_rand" => DirectReplaceUnaryOperation::from_func_type(DirectReplaceUnaryOperationT::Rand, base_ptr, store_type),
            "m_atan2" => DirectReplaceBinaryOperation::from_func_type(DirectReplaceBinaryOperationT::Atan2, base_ptr, store_type),
            "m_max" => DirectReplaceBinaryOperation::from_func_type(DirectReplaceBinaryOperationT::Max, base_ptr, store_type),
            "m_min" => DirectReplaceBinaryOperation::from_func_type(DirectReplaceBinaryOperationT::Min, base_ptr, store_type),
            "m_sleep" => (MipsOperation::Sleep(Sleep { op_1: MipsOperand::VariableMapping(VariableMapping::from_register_number(base_ptr, VarType::Float)) }), 1),
            "m_yield" => (MipsOperation::Yield(Yield {}), 0),
            "load" => (MipsOperation::Load(Load { 
                store: VariableMapping::from_register_number(base_ptr, store_type),
                device_id: MipsOperand::from_register_number(base_ptr + 1, VarType::Int),
                device_variable_type: MipsOperand::from_register_number(base_ptr + 2, VarType::Int) 
            }), 3),
            "load_reagent" => (MipsOperation::LoadReagent(LoadReagent { 
                store: VariableMapping::from_register_number(base_ptr, store_type),
                device_id: MipsOperand::from_register_number(base_ptr + 1, VarType::Int),
                reagent_mode: MipsOperand::from_register_number(base_ptr + 2, VarType::Int),
                reagent_hash: MipsOperand::from_register_number(base_ptr + 3, VarType::Int)
            }), 4),
            "load_slot" => (MipsOperation::LoadSlot(LoadSlot { 
                store: VariableMapping::from_register_number(base_ptr, store_type),
                device_id: MipsOperand::from_register_number(base_ptr + 1, VarType::Int),
                slot_index: MipsOperand::from_register_number(base_ptr + 2, VarType::Int),
                slot_variable_type: MipsOperand::from_register_number(base_ptr + 3, VarType::Int)
            }), 4),
            "load_batch" => (MipsOperation::LoadBatch(LoadBatch { 
                store: VariableMapping::from_register_number(base_ptr, store_type), 
                device_type: MipsOperand::from_register_number(base_ptr + 1, VarType::Int),
                device_variable_type: MipsOperand::from_register_number(base_ptr + 2, VarType::Int),
                batch_mode: MipsOperand::from_register_number(base_ptr + 3, VarType::Int) 
            }), 4),
            "load_batch_slot" => (MipsOperation::LoadBatchSlot(LoadBatchSlot {
                store: VariableMapping::from_register_number(base_ptr, store_type),
                device_type: MipsOperand::from_register_number(base_ptr + 1, VarType::Int),
                slot_index: MipsOperand::from_register_number(base_ptr + 2, VarType::Int),
                slot_variable_type: MipsOperand::from_register_number(base_ptr + 3, VarType::Int),
                batch_mode: MipsOperand::from_register_number(base_ptr + 4, VarType::Int) 
            }), 5),
            "load_batch_with_name" => (MipsOperation::LoadBatchWithName(LoadBatchWithName {
                store: VariableMapping::from_register_number(base_ptr, store_type),
                device_type: MipsOperand::from_register_number(base_ptr + 1, VarType::Int),
                device_name: MipsOperand::from_register_number(base_ptr + 2, VarType::Int),
                device_variable_type: MipsOperand::from_register_number(base_ptr + 3, VarType::Int),
                batch_mode: MipsOperand::from_register_number(base_ptr + 4, VarType::Int) 
            }), 5),
            "load_batch_with_name_slot" => (MipsOperation::LoadBatchWithNameSlot(LoadBatchWithNameSlot {
                store: VariableMapping::from_register_number(base_ptr, store_type),
                device_type: MipsOperand::from_register_number(base_ptr + 1, VarType::Int),
                device_name: MipsOperand::from_register_number(base_ptr + 2, VarType::Int),
                slot_index: MipsOperand::from_register_number(base_ptr + 3, VarType::Int),
                slot_variable_type: MipsOperand::from_register_number(base_ptr + 4, VarType::Int),
                batch_mode: MipsOperand::from_register_number(base_ptr + 5, VarType::Int) 
            }), 6),
            "store" => (MipsOperation::Store(Store {
                device_id: MipsOperand::from_register_number(base_ptr, VarType::Int),
                device_variable_type: MipsOperand::from_register_number(base_ptr + 1, VarType::Int),
                source: MipsOperand::from_register_number(base_ptr + 2, VarType::Int) 
            }), 3),
            "store_slot" => (MipsOperation::StoreSlot(StoreSlot {
                device_id: MipsOperand::from_register_number(base_ptr, VarType::Int),
                slot_index: MipsOperand::from_register_number(base_ptr + 1, VarType::Int),
                slot_variable_type: MipsOperand::from_register_number(base_ptr + 2, VarType::Int),
                source: MipsOperand::from_register_number(base_ptr + 3, VarType::Int) 
            }), 4),
            "store_batch" => (MipsOperation::StoreBatch(StoreBatch {
                device_type: MipsOperand::from_register_number(base_ptr, VarType::Int),
                device_variable_type: MipsOperand::from_register_number(base_ptr + 1, VarType::Int),
                source: MipsOperand::from_register_number(base_ptr + 2, VarType::Int) 
            }), 3),
            "store_batch_with_name" => (MipsOperation::StoreBatchWithName(StoreBatchWithName {
                device_type: MipsOperand::from_register_number(base_ptr, VarType::Int),
                device_name: MipsOperand::from_register_number(base_ptr + 1, VarType::Int),
                device_variable_type: MipsOperand::from_register_number(base_ptr + 2, VarType::Int),
                source: MipsOperand::from_register_number(base_ptr + 3, VarType::Int) 
            }), 4),
            "store_batch_slot" => (MipsOperation::StoreBatchSlot(StoreBatchSlot {
                device_type: MipsOperand::from_register_number(base_ptr, VarType::Int),
                slot_index: MipsOperand::from_register_number(base_ptr + 1, VarType::Int),
                slot_variable_type: MipsOperand::from_register_number(base_ptr + 2, VarType::Int),
                source: MipsOperand::from_register_number(base_ptr + 3, VarType::Int) 
            }), 4),
            _ => return None
        };
        Some(op)
    }
}