#[derive(Clone)]
pub(super) struct RegisterMapping {
    pub(super) reg_no: usize
}

#[derive(Clone)]
pub(super) struct StackMapping {
    pub(super) relative_addr: usize,
}

#[derive(Clone)]
pub(super) enum VariableMapping {
    RegisterMapping(RegisterMapping),
    StackMapping(StackMapping),
    StackPointer,
    ReturnAddress
}

impl VariableMapping {
    pub(super) fn from_register_number(reg_no: usize) -> Self {
        VariableMapping::RegisterMapping(RegisterMapping { reg_no })
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
    pub(super) fn from_register_number(reg_no: usize) -> Self {
        MipsOperand::VariableMapping(VariableMapping::RegisterMapping(RegisterMapping { reg_no }))
    }

    pub(super) fn from_string_literal(str: String) -> Self {
        MipsOperand::Literal(str)
    }

    pub(super) fn from_number_literal(num: f32) -> Self {
        MipsOperand::Literal(format!("{}", num))
    }
}

#[derive(Clone)]
pub(super) enum MipsOperation {
    Move(Move),
    Add(Add),
    Sub(Sub),
    Mul(Mul),
    Div(Div),
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