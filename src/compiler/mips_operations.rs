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
            return Ok(VarType::Int)
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
pub(super) struct Pop {
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
pub(super) struct JumpReg {
    pub(super) reg: MipsOperand
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
pub(super) enum DirectReplaceUnaryOperandT {
    Sin,
    Cos,
    Tan,
    Asin,
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
    Rand
}

#[derive(Clone)]
pub(super) struct DirectReplaceUnaryOperand {
    pub(super) func_type: DirectReplaceUnaryOperandT,
    pub(super) store: VariableMapping,
    pub(super) op_1: MipsOperand
}

#[derive(Clone)]
pub(super) enum DirectReplaceBinaryOperandT {
    Max,
    Min,
    Atan2
}

#[derive(Clone)]
pub(super) struct DirectReplaceBinaryOperand {
    pub(super) func_type: DirectReplaceBinaryOperandT,
    pub(super) store: VariableMapping,
    pub(super) op_1: MipsOperand,
    pub(super) op_2: MipsOperand
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
        return VarType::Int;
    }

    pub(super) fn implicit_conversion(&self, var_type: VarType) -> (MipsOperand, Vec<MipsOperation>) {
        let current = self.get_var_type();
        match self {
            MipsOperand::VariableMapping(var) => {
                match var {
                    VariableMapping::RegisterMapping(reg) => {
                        let new_op = MipsOperand::from_register_number(reg.reg_no, var_type.clone());
                        if var_type == VarType::Int && current != VarType::Int {
                            return (new_op.clone(), Vec::from([MipsOperation::Floor(Floor { store: var.clone(), op_1: new_op.clone()})]));
                        }
                        else {
                            return (new_op, Vec::new());
                        }
                        
                    },
                    VariableMapping::StackMapping(sta) => return (MipsOperand::from_stack_addr(sta.relative_addr, var_type), Vec::new()),
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
                } else {
                    if !lit.contains('.') {
                        (MipsOperand::from_string_literal(lit.clone() + ".0"), Vec::new())
                    }
                    else {
                        (self.clone(), Vec::new())
                    }
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
    Return(Return)
}