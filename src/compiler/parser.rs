use crate::{compiler::scanner::{Token, TokenType}, error_handler::{print_stage, report}};
use super::scanner::tok_type_string;

struct ParserError;

#[derive(Clone)]
pub struct BinaryExpr<'a> {
    pub(super) left: Box<Expr<'a>>,
    pub(super) right: Box<Expr<'a>>,
    pub(super) operator: Token,
}

#[derive(Clone)]
pub struct CallExpr<'a> {
    pub(super) call: FuncCallStmt<'a>
}

#[derive(Clone)]
pub struct GroupingExpr<'a> {
    pub(super) expression: Box<Expr<'a>>,
}

#[derive(Clone)]
pub struct LiteralExpr<'a> {
    pub(super) value: Option<&'a Token>,
}

#[derive(Clone)]
pub struct UnaryExpr<'a> {
    pub(super) operator: Token,
    pub(super) right: Box<Expr<'a>>,
}

#[derive(Clone)]
pub struct VariableExpr<'a> {
    pub(super) name: &'a Token,
}

#[derive(Clone)]
pub struct ArrayIntialiserExpr<'a> {
    pub(super) elements: Vec<Expr<'a>>,
    pub(super) brace_tok: &'a Token,
    pub(super) step_size: usize,
    pub(super) var_type: &'a Token
}

#[derive(Clone)]
pub struct StoredValueExpr {
    pub(super) value: usize
}

#[derive(Clone)]
pub enum Expr<'a> {
    Binary(BinaryExpr<'a>),
    Call(CallExpr<'a>),
    Grouping(GroupingExpr<'a>),
    Literal(LiteralExpr<'a>),
    Unary(UnaryExpr<'a>),
    Variable(VariableExpr<'a>),
    StoredValue(StoredValueExpr),
    ArrayIntialiser(ArrayIntialiserExpr<'a>)
}

pub struct BlockStmt<'a> {
    pub(super) statements: Vec<Stmt<'a>>
}

pub struct ExprStmt<'a> {
    pub(super) expression: Box<Expr<'a>>
}

pub struct FuncStmt<'a> {
    pub(super) name: &'a Token,
    pub(super) ret_type: &'a Token,
    pub(super) params: Vec<Stmt<'a>>,
    pub(super) body: Box<Stmt<'a>>,
    pub(super) is_main: bool
}

pub struct IfStmt<'a> {
    pub(super) conditions: Vec<Expr<'a>>,
    pub(super) branches: Vec<Stmt<'a>>
}

pub struct ReturnStmt<'a> {
    pub(super) value: Box<Expr<'a>>,
    pub(super) ret_type: Token,
}

pub struct VarStmt<'a> {
    pub(super) var_name: &'a Token,
    pub(super) var_type: &'a Token,
    pub(super) initialiser: Option<Box<Expr<'a>>>,
    pub(super) var_sizes: Vec<usize>,
    pub(super) is_array: bool
}

pub struct AssignStmt<'a> {
    pub(super) lvalue_expr: Box<Expr<'a>>,
    pub(super) binding: Box<Expr<'a>>,
    pub(super) lvalue_var: Token
}

pub struct WhileStmt<'a> {
    pub(super) condition: Box<Expr<'a>>,
    pub(super) body: Box<Stmt<'a>>
}

pub struct ForStmt<'a> {
    pub(super) variant: Option<Box<Stmt<'a>>>,
    pub(super) condition: Box<Expr<'a>>,
    pub(super) incrementer: Option<Box<Stmt<'a>>>,
    pub(super) body: Box<Stmt<'a>>
}

#[derive(Clone)]
pub struct FuncCallStmt<'a> {
    pub(super) name: &'a Token,
    pub(super) params: Vec<Expr<'a>>,
}

pub enum Stmt<'a> {
    Block(BlockStmt<'a>),
    Expression(ExprStmt<'a>),
    Function(FuncStmt<'a>),
    If(IfStmt<'a>),
    Return(ReturnStmt<'a>),
    Variable(VarStmt<'a>),
    While(WhileStmt<'a>),
    For(ForStmt<'a>),
    Assign(AssignStmt<'a>),
    FunctionCall(FuncCallStmt<'a>)
}

impl<'a> Expr<'a> {
    pub(super) fn lvalue_is_derefed(&'a self) -> Option<&'a Expr> {
        match self {
            Expr::Unary(e) => if e.operator.tok_type == TokenType::Star { Some(&e.right) } else { None },
            Expr::Grouping(e) => e.expression.lvalue_is_derefed(),
            _ => None
        }
    }

    // This function aims to match the following definition for lvalue expressions:
    // *(lvalue + expr) | *(lvalue - expr) | *(expr + lvalue) | lvalue | *lvalue | *(lvalue) | var_name
    pub(super) fn match_l_value(&self, derefed: bool) -> Option<Token> {
        match self {
            Expr::Binary(e) => {
                if !derefed {
                    return None;
                }
                let left_lvalue = e.left.match_l_value(false);
                let operator = e.operator.tok_type;
                if left_lvalue.is_some() && (operator == TokenType::Plus || operator == TokenType::Minus) {
                    return left_lvalue; // *(lvalue + expr) | *(lvalue - expr)
                }
                else if left_lvalue.is_none() && operator == TokenType::Plus {
                    return e.right.match_l_value(false);
                }
                None
            },
            Expr::Call(_) => None,
            Expr::Grouping(e) => e.expression.match_l_value(derefed),
            Expr::Literal(_) => None,
            Expr::Unary(e) => {
                let derefed = e.operator.tok_type == TokenType::Star;
                e.right.match_l_value(derefed)
            },
            Expr::Variable(v) => Some(v.name.clone()),
            Expr::StoredValue(_) => None,
            Expr::ArrayIntialiser(_) => None,
        }
    }

    // Computes the maximum length of each node at each level in the tree
    fn array_initialiser_sizes(&self, depth: usize, max_lengths: &mut Vec<usize>) {
        if let Expr::ArrayIntialiser(init) = self {
            for element in &init.elements {
                element.array_initialiser_sizes(depth + 1, max_lengths);
            }
            while depth >= max_lengths.len() { max_lengths.push(0); }
            max_lengths[depth] = max_lengths[depth].max(init.elements.len());
        }
    }

    fn propogate_sizes_and_type(&mut self, depth: usize, sizes: &Vec<usize>, var_type: &'a Token) {
        if let Expr::ArrayIntialiser(init) = self {
            for element in &mut init.elements {
                element.propogate_sizes_and_type(depth + 1, sizes, var_type)
            }
            init.step_size = sizes[(depth+1)..sizes.len()].iter().product::<usize>();
            init.var_type = var_type;
        }
    }
}

pub fn pretty_print_stmt(stmt: &Stmt, depth: usize) -> String {
    match stmt {
        Stmt::Function(s) => {
            let mut str = format!("{} {}(", s.ret_type.lexeme, s.name.lexeme);
            for (i, param) in s.params.iter().enumerate() {
                match param {
                    Stmt::Variable(p) => {
                        str.push_str(&format!("{} {}", p.var_type.lexeme, p.var_name.lexeme));
                        if s.params.len() > 1 && i <= s.params.len() - 2 {
                            str.push_str(", ");
                        }
                    },
                    _ => panic!("Internal compiler error.")
                }

            }
            str.push(')');
            str.push_str(&pretty_print_stmt(&s.body, depth));
            str
        },
        Stmt::Variable(s) => {
            let mut str = format!("{} {}", s.var_type.lexeme, s.var_name.lexeme);
            match s.initialiser.clone() {
                Some(init) => str.push_str(&format!(" = {};\n", ast_pretty_printer(*init))),
                None => str.push_str(";\n"),
            }
            str
        },
        Stmt::Expression(s) => {
            ast_pretty_printer(*s.expression.clone())
        }
        Stmt::While(s) => {
            let mut str = format!("while ({})", ast_pretty_printer(*s.condition.clone()));
            str.push_str(&pretty_print_stmt(&s.body, depth));
            str
        }
        Stmt::Block(s) => {
            let depth = depth + 1;
            let mut str = String::from("");
            str.push_str(" {\n");
            for stmt in s.statements.iter() {
                str.push_str(&format!("{}{}", indent_depth(depth), pretty_print_stmt(stmt, depth)));
            }
            str.push_str(&format!("{}}}\n", indent_depth(depth - 1)));
            str
        }
        Stmt::For(s) => {
            let variant_str = { 
                match &s.variant {
                    Some(v) => pretty_print_stmt(v, depth),
                    None => String::from("  "),
                }
            };

            let incrementer_str = {
                match &s.incrementer {
                    Some(i) => pretty_print_stmt(i, depth),
                    None => String::from("  "),
                }
            };
            
            // Remove that last two characters ';' and '\n'
            let mut str = format!("for ({}; {}; {})", 
                &variant_str[..(variant_str.len() - 2)], 
                ast_pretty_printer(*s.condition.clone()), 
                &incrementer_str[..(incrementer_str.len() - 2)]);

            str.push_str(&pretty_print_stmt(&s.body, depth));
            str
        },
        Stmt::If(s) => {
            let mut str = String::from("");
            for i in 0..s.branches.len() {
                let keyword;
                let mut keyword_depth = depth;
                if i == 0 { keyword = String::from("if"); keyword_depth = 0 }
                else { keyword = String::from("else if"); }

                if i != s.branches.len() - 1 || s.branches.len() == 1 {
                    let first_cond_str = {
                        match s.conditions.get(i) {
                            Some(c) => ast_pretty_printer(c.clone()),
                            None => todo!(),
                        }
                    };
                    str.push_str(&format!("{}{} ({})", indent_depth(keyword_depth), keyword, first_cond_str));
                }
                else {
                    str.push_str(&format!("{}else", indent_depth(keyword_depth)));
                }

                match s.branches.get(i) {
                    Some(b) => str.push_str(&pretty_print_stmt(b, depth)),
                    None => todo!(),
                }
            }
            str
        },
        Stmt::Assign(s) => format!("{} = {};\n", ast_pretty_printer(*s.lvalue_expr.clone()), ast_pretty_printer(*s.binding.clone())),
        Stmt::Return(s) => format!("return {};\n", ast_pretty_printer(*s.value.clone())),
        Stmt::FunctionCall(s) => {
            let mut str = format!("{}(", s.name.lexeme);
            for (i, expr) in s.params.iter().enumerate() {
                str.push_str(&ast_pretty_printer(*Box::new(expr.clone())));
                if i != s.params.len() - 1 {
                    str.push_str(", ");
                }
            }
            str.push_str(");\n");
            str
        }
    }
}

fn indent_depth(depth: usize) -> String {
    let mut str = String::from("");
    let mut i = 0;
    while i < depth {
        str.push_str("    ");
        i += 1;
    }
    str
}

fn parenthesize_expr(expr: Expr) -> String {
    match expr {
        Expr::Binary(e) => parenthesize(e.operator.lexeme.clone(), Vec::from([*e.left, *e.right])),
        Expr::Grouping(e) => parenthesize(String::from("group"), Vec::from([*e.expression])),
        Expr::Literal(e) => {
            match e.value {
                Some(v) => v.lexeme.clone(),
                None => String::from("")
            }
        },
        Expr::Unary(e) => parenthesize(e.operator.lexeme.clone(), Vec::from([*e.right])),
        Expr::Variable(e) => parenthesize(e.name.lexeme.clone(), Vec::from([])),
        Expr::Call(e) => {
            let call_stmt = Stmt::FunctionCall(e.call.clone());
            let str = pretty_print_stmt(&call_stmt, 0);
            str[..str.len() - 2].to_string()
        },
        Expr::StoredValue(sto) => format!("{}", sto.value),
        Expr::ArrayIntialiser(array_init) => parenthesize(String::from("{}"), array_init.elements),
    }
} 

fn parenthesize(name: String, exprs: Vec<Expr>) -> String {
    let mut result = String::from("(");
    result.push_str(&name);
    for expr in exprs {
        result.push(' ');
        result.push_str(&parenthesize_expr(expr));
    }
    result.push(')');
    result
}

pub fn ast_pretty_printer(expr: Expr) -> String {
    parenthesize_expr(expr)
}

pub fn parse(tokens: &[Token], verbose: bool) -> Vec<Stmt> {
    let mut statements: Vec<Stmt> = vec![];
    let mut current = 0;

    let mut parsed_str = String::from("");

    while !is_at_eof(current, tokens) {
        let stmt_res = parse_tokens_and_generate_stmt(current, tokens, 0, false, &tokens[0]);
        match stmt_res {
            Ok((stmt, c)) => {
                current = c;
                parsed_str.push_str(&pretty_print_stmt(&stmt, 0));
                statements.push(stmt);
            }
            Err(_) => panic!("Internal compiler error.")
        }
    }

    if verbose {
        print_stage(parsed_str, String::from("PARSED CODE:"));
    }
    
    statements
}

fn block_statement<'a>(current: usize, tokens: &'a [Token], depth: usize, ret_type: &'a Token) -> Result<(BlockStmt<'a>, usize), ParserError> {
    let mut current = current;

    // Start of block
    if !(match_token(&mut current, tokens, &Vec::from([TokenType::LeftBrace]))) {
        parsing_error(peek(current, tokens), String::from("Expected '{'."));
        return Err(ParserError);
    }

    let mut stmts: Vec<Stmt> = Vec::new();
    while !(match_token(&mut current, tokens, &Vec::from([TokenType::RightBrace]))) {
        let stmt_res = parse_tokens_and_generate_stmt(current, tokens, depth + 1, false, ret_type);
        match stmt_res {
            Ok((stmt, c)) => { 
                stmts.push(stmt); 
                current = c;
            },
            Err(_) => return Err(ParserError)
        }
    }
    Ok((BlockStmt { statements: stmts }, current))
}

fn for_statement<'a>(current: usize, tokens: &'a [Token], ret_type: &'a Token) -> Result<(Stmt<'a>, usize), ParserError> {
    // Structure:
    // for (initialisation; condition; expression) Block
    // All three are optional 
    //      - if no condition expression set to True
    //      - initialisation may be an assignment or a variable declaration with initialisation
    let mut current = current;
    (_, current) = consume_token(TokenType::LeftParen, String::from("Expect '(' after for."), current, tokens);
    
    let init;
    if match_token(&mut current, tokens, &Vec::from([TokenType::Semicolon])) {
        // No initialiser
        init = None;
    }
    else {
        let initialisation = parse_tokens_and_generate_stmt(current, tokens, 2, true, ret_type);
        match initialisation {
            Ok((stmt, c)) => {
                match stmt {
                    Stmt::Variable(ref var_stmt) => { if var_stmt.initialiser.is_none() { return Err(ParserError); } },
                    Stmt::Assign(_) => {},
                    _ => return Err(ParserError)
                }
                init = Some(Box::new(stmt));
                current = c;
            },
            Err(_) => return Err(ParserError)
        }
        (_, current) = consume_token(TokenType::Semicolon, String::from("Expect ';' after initialiser."), current, tokens);
    }

    let condition;
    if match_token(&mut current, tokens, &Vec::from([TokenType::Semicolon])) {
        condition = Box::new(Expr::StoredValue(StoredValueExpr { value: 1 } ));
    }
    else {
        match expression_statement(current, tokens) {
            Ok((expr, c)) => (condition, current) = (expr, c),
            Err(_) => return Err(ParserError),
        }
        (_, current) = consume_token(TokenType::Semicolon, String::from("Expect ';' after condition."), current, tokens);
    }

    let incrementer;
    if match_token(&mut current, tokens, &Vec::from([TokenType::RightParen])) {
        incrementer = None;
    }
    else {
        match parse_tokens_and_generate_stmt(current, tokens, 2, true, ret_type) {
            Ok((stmt, c)) => {
                if let Stmt::Assign(_) = stmt {
                    incrementer = Some(Box::new(stmt));
                    current = c;
                }
                else {
                    return Err(ParserError);
                }
            },
            Err(_) => return Err(ParserError),
        }
        (_, current) = consume_token(TokenType::RightParen, String::from("Expect ')' after expression."), current, tokens);
    }

    let body;
    match block_statement(current, tokens, 2, ret_type) {
        Ok((b, c)) => (body, current) = (Box::new(Stmt::Block(b)), c),
        Err(_) => return Err(ParserError),
    }

    return Ok((Stmt::For(ForStmt {
        variant: init,
        condition,
        incrementer,
        body
    }), current));
}

fn while_statement<'a>(current: usize, tokens: &'a [Token], ret_type: &'a Token) -> Result<(Stmt<'a>, usize), ParserError> {
    let mut current = current;
    (_, current) = consume_token(TokenType::LeftParen, String::from("Expect '(' after while."), current, tokens);

    let condition;
    match expression(current, tokens) {
        Ok((cond, c)) => (condition, current) = (cond, c),
        Err(_) => return Err(ParserError)
    }

    (_, current) = consume_token(TokenType::RightParen, String::from("Expect ')' after condition."), current, tokens);

    let body;
    match block_statement(current, tokens, 2, ret_type) {
        Ok((b, c)) => (body, current) = (b, c),
        Err(_) => return Err(ParserError)
    }

    return Ok((Stmt::While(WhileStmt {
        condition,
        body: Box::new(Stmt::Block(body)),
    }), current));
}

fn if_statement<'a>(current: usize, tokens: &'a [Token], ret_type: &'a Token) -> Result<(Stmt<'a>, usize), ParserError> {
    // List of condition block pairs
    let mut current = current;
    let mut conditions = Vec::new();
    let mut branches = Vec::new();

    let mut another_branch = true;
    while another_branch {
        if match_token(&mut current, tokens, &Vec::from([TokenType::LeftParen])) {
            match expression_statement(current, tokens) {
                Ok((condition, c)) =>  { 
                    conditions.push(*condition);
                    current = c;
                },
                Err(_) => return Err(ParserError)
            }
        
            (_, current) = consume_token(TokenType::RightParen, String::from("Expect ')' after condition."), current, tokens);
            
            match block_statement(current, tokens, 2, ret_type) {
                Ok((branch, c)) => {
                    branches.push(Stmt::Block(branch));
                    current = c;
                },
                Err(_) => return Err(ParserError)
            }
        }

        another_branch = match_token(&mut current, tokens, &Vec::from([TokenType::Else])) && match_token(&mut current, tokens, &Vec::from([TokenType::If]));
    }

    if previous(current, tokens).tok_type == TokenType::Else {
        conditions.push(Expr::Literal(LiteralExpr { value: None }));
        match block_statement(current, tokens, 2, ret_type) {
            Ok((branch, c)) => {
                branches.push(Stmt::Block(branch));
                current = c;
            },
            Err(_) => return Err(ParserError)
        }
    }

    return Ok((Stmt::If(IfStmt {
        conditions,
        branches
    }), current));
}

fn function_call_statement<'a>(current: usize, tokens: &'a [Token], identifier: &'a Token, inline: bool) -> Result<(Stmt<'a>, usize), ParserError> {
    let mut current = current;
    let mut params: Vec<Expr> = Vec::new();
    while !match_token(&mut current, tokens, &Vec::from([TokenType::RightParen])) {
        match expression_statement(current, tokens) {
            Ok((expr, c)) => {
                params.push(*expr);
                current = c;
            },
            Err(_) => return Err(ParserError),
        }

        let delim_tok = peek(current, tokens);
        if delim_tok.tok_type == TokenType::Comma {
            (_, current) = consume_token(TokenType::Comma, String::from(""), current, tokens)
        }
        else if delim_tok.tok_type != TokenType::RightParen {
            parsing_error(delim_tok, String::from("Expected ')'."));
            return Err(ParserError);
        }
    }

    if !inline {
        (_, current) = consume_token(TokenType::Semicolon, String::from("Expect ';' after function call."), current, tokens);
    }

    return Ok((Stmt::FunctionCall(FuncCallStmt {
        name: identifier,
        params
    }), current));
} 



fn assignment_statement(current: usize, tokens: &[Token], inline_statement: bool) -> Result<(Stmt, usize), ParserError> {
    let mut current = current;
    let lhs_expr;
    (lhs_expr, current) = expression(current - 1, tokens)?;

    let lvalue_var = match lhs_expr.match_l_value(false) {
        Some(v) => v,
        None => {
            parsing_error(previous(current, tokens), String::from("Expected lvalue expression"));
            return Err(ParserError);
        }
    };
    
    if match_token(&mut current, tokens, &Vec::from([TokenType::Equal, TokenType::AndEqual, TokenType::OrEqual, TokenType::XorEqual, TokenType::PlusEqual, TokenType::StarEqual, TokenType::MinusEqual, TokenType::SlashEqual, TokenType::PercentEqual])) {
        let assignment_type = previous(current, tokens);
        if let Ok((mut binding, current)) = expression_statement(current, tokens) {
            let mut current = current;
            if !inline_statement {
                (_, current) = consume_token(TokenType::Semicolon, String::from("Expect ';' after assignment."), current, tokens);
            }

            let assigment_operator = match assignment_type.tok_type {
                TokenType::AndEqual => TokenType::BitwiseAnd, 
                TokenType::OrEqual => TokenType::BitwiseOr, 
                TokenType::XorEqual => TokenType::BitwiseXor, 
                TokenType::PercentEqual => TokenType::Percent, 
                TokenType::PlusEqual => TokenType::Plus, 
                TokenType::MinusEqual => TokenType::Minus, 
                TokenType::StarEqual => TokenType::Star, 
                TokenType::SlashEqual => TokenType::Slash,
                _ => assignment_type.tok_type
            };

            if assignment_type.tok_type != TokenType::Equal {
                let op_str = tok_type_string(assigment_operator);
                let operator_token = Token { tok_type: assigment_operator, lexeme: op_str.clone(), literal: op_str.clone(), line_no: assignment_type.line_no };
                binding = Box::new(Expr::Binary(BinaryExpr { 
                    left: lhs_expr.clone(),
                    right: binding,
                    operator: operator_token }
                ));
            }

            return Ok((Stmt::Assign(AssignStmt {
                lvalue_expr: lhs_expr,
                binding,
                lvalue_var
            }), current));
        }
        else {
            return Err(ParserError);
        }
    }
    Err(ParserError)
}

fn generate_statement<'a>(current: usize, tokens: &'a [Token], inline_statement: bool, ret_type: &'a Token) -> Result<(Stmt<'a>, usize), ParserError> {
    let mut current = current;
    if match_token(&mut current, tokens, &Vec::from([TokenType::While])) { return while_statement(current, tokens, ret_type); }
    else if match_token(&mut current, tokens, &Vec::from([TokenType::For])) { return for_statement(current, tokens, ret_type); }
    else if match_token(&mut current, tokens, &Vec::from([TokenType::If])) { return if_statement(current, tokens, ret_type); }
    else if match_token(&mut current, tokens, &Vec::from([TokenType::Identifier, TokenType::Star, TokenType::LeftParen])) {
        let identifier = previous(current, tokens);
        if identifier.is_alphaneumeric() && match_token(&mut current, tokens, &Vec::from([TokenType::LeftParen])) {
            return function_call_statement(current, tokens, identifier, false);
        }
        else {
            return assignment_statement(current, tokens, inline_statement);
        }
    }
    else if match_token(&mut current, tokens, &Vec::from([TokenType::Return])) {
        if let Ok((binding, current)) = expression_statement(current, tokens) {
            let mut current = current;
            (_, current) = consume_token(TokenType::Semicolon, String::from("Expect ';' after return expression."), current, tokens);
            return Ok((Stmt::Return(ReturnStmt {
                value: binding,
                ret_type: ret_type.clone()
            }), current))
        }
    }

    println!("{}", peek(current, tokens).lexeme);
    Err(ParserError)
}

fn expression_statement(current: usize, tokens: &[Token]) -> Result<(Box<Expr>, usize), ParserError> {
    return expression(current, tokens); 
}

fn parse_tokens_and_generate_stmt<'a>(current: usize, tokens: &'a [Token], depth: usize, inline_statement: bool, ret_type: &'a Token) -> Result<(Stmt<'a>, usize), ParserError> {
    let mut current = current;
    if match_token(&mut current, tokens, &Vec::from([TokenType::Int, TokenType::Float, TokenType::Double, TokenType::Char])) {
        let var_dec_opt = variable_declaration(current, tokens, inline_statement, depth);
        match var_dec_opt {
            Ok((stmt, c)) => {
                Ok((stmt, c))
            },
            Err(_) => {
                current = error_synchronise(current, tokens);
                return Ok((Stmt::Expression(ExprStmt {
                    expression: Box::new(Expr::StoredValue(StoredValueExpr { value: 1 }))
                }), current));
            }
        }
    }
    else {
        let stmt_opt = generate_statement(current, tokens, inline_statement, ret_type);
        match stmt_opt {
            Ok((s, c)) => Ok((s, c)),
            Err(_) => {
                current = error_synchronise(current, tokens);
                return Ok((Stmt::Expression(ExprStmt {
                    expression: Box::new(Expr::StoredValue(StoredValueExpr { value: 1 }))
                }
                ), current));
            }
        }
    }
}

// Depth argument prevents nested functions
fn function_declaration<'a>(current: usize, tokens: &'a [Token], depth: usize, func_name: &'a Token, ret_type: &'a Token) -> Result<(Stmt<'a>, usize), ParserError> {
    let mut current = current;

    if depth > 1 {
        parsing_error(peek(current, tokens), String::from("Nested functions are not allowed."));
        return Err(ParserError);
    }

    // Match arg types
    let mut args: Vec<Stmt> = Vec::new();
    while !match_token(&mut current, tokens, &Vec::from([TokenType::RightParen])) && match_token(&mut current, tokens, &Vec::from([TokenType::Int, TokenType::Float, TokenType::Char, TokenType::Double])) {
        let arg_res = variable_declaration(current, tokens, true, depth);
        match arg_res {
            Ok((arg, c)) => {
                args.push(arg);
                current = c;
            }
            Err(_) => return Err(ParserError)
        }
        
        let delim_tok = peek(current, tokens);
        if delim_tok.tok_type == TokenType::Comma {
            (_, current) = consume_token(TokenType::Comma, String::from(""), current, tokens)
        }
        else if delim_tok.tok_type != TokenType::RightParen {
            parsing_error(delim_tok, String::from("Expected ')'."));
            return Err(ParserError);
        }
    }

    let block_stmts = block_statement(current, tokens, depth, ret_type);
    let is_main = func_name.lexeme == "main";
    match block_stmts {
        Ok((stmts, c)) => {
            Ok((Stmt::Function(FuncStmt {
                name: func_name,
                params: args,
                body: Box::new(Stmt::Block(stmts)),
                ret_type,
                is_main
            }), c))
        },
        Err(_) => Err(ParserError)
    }
}

// TODO: This function is a mess
fn variable_size(init: Option<Box<Expr>>, specified_lens: &Vec<Option<usize>>, var_token: &Token) -> Result<Vec<usize>, ParserError> {
    let mut result_lengths = vec![];
    let mut valid_length_specified = true;
    let mut lens = Vec::new();

    for opt_len in specified_lens {
        match opt_len {
            Some(len) => lens.push(*len),
            None => {
                valid_length_specified = false;
                break;
            },
        }
    }

    if let Some(var_init) = init {
        if let Expr::ArrayIntialiser(_) = *var_init {}
        else {
            return Ok(Vec::from([1]));
        }

        let mut initialiser_lengths: Vec<usize> = Vec::new();
        var_init.array_initialiser_sizes(0, &mut initialiser_lengths);
        let specified_dims = lens.len();

        while initialiser_lengths.len() < specified_dims { initialiser_lengths.push(0); }
        while lens.len() < initialiser_lengths.len() { lens.push(0); }

        for i in 0..initialiser_lengths.len() {
            if initialiser_lengths[i] > lens[i] && specified_lens[i].is_some() {
                parsing_error(var_token, String::from("Array initialiser dimensions do not match those specified."));
                return Err(ParserError);
            }
            else if specified_lens[i].is_none() {
                result_lengths.push(initialiser_lengths[i])
            } else {
                result_lengths.push(lens[i])
            }
        }
    }

    if valid_length_specified {
        result_lengths = lens;
    }
    
    result_lengths.push(1);
    Ok(result_lengths)
}

fn variable_declaration(current: usize, tokens: &[Token], is_func_arg: bool, depth: usize) -> Result<(Stmt, usize), ParserError> {
    let mut current = current;
    let var_type = previous(current, tokens);
    let mut is_array = false;

    // Optional Stars for pointers
    while match_token(&mut current, tokens, &Vec::from([TokenType::Star])) {}

    let var_name;
    (var_name, current) = consume_token(TokenType::Identifier, String::from("Expect variable name."), current, tokens);

    // Array bounds initialisation
    let mut array_lens = Vec::new();
    while match_token(&mut current, tokens, &Vec::from([TokenType::LeftSquareBrace])) {
        is_array = true;
        let len = peek(current, tokens);
        if match_token(&mut current, tokens, &Vec::from([TokenType::Number])) {
            match len.lexeme.parse::<usize>() {
                Ok(len_i) => array_lens.push(Some(len_i)),
                Err(_) => { 
                    parsing_error(len, String::from("Expect integral value array size."));
                    return Err(ParserError)
                }
            }
        }
        else {
            array_lens.push(None)
        }
        (_, current) = consume_token(TokenType::RightSquareBrace, String::from("Expect ']' after array size."), current, tokens);
    }

    let mut initialiser;
    if match_token(&mut current, tokens, &Vec::from([TokenType::Equal])) {
        let initialiser_result = expression_statement(current, tokens);
        match initialiser_result {
            Ok((init, c)) => (initialiser, current) = (Some(init), c),
            Err(_) => return Err(ParserError)
        }
    }
    else if match_token(&mut current, tokens, &Vec::from([TokenType::LeftParen])) {
        return function_declaration(current, tokens, depth + 1, var_name, var_type);
    }
    else {
        initialiser = None;
    }

    if !is_func_arg {
        (_, current) = consume_token(TokenType::Semicolon, String::from("Expect ';' after variable declaration."), current, tokens);
    }

    let var_sizes = variable_size(initialiser.clone(), &array_lens, var_name)?;
    if let Some(ref mut init) = initialiser { init.propogate_sizes_and_type(0, &var_sizes, var_type) }
    
    return Ok((Stmt::Variable(VarStmt {
        var_name,
        var_type,
        initialiser,
        var_sizes,
        is_array
    }), current));

}

fn error_synchronise(current: usize, tokens: &[Token]) -> usize {
    let mut current = current;
    next_token(&mut current, tokens);

    // Iterate until we locate a keyword or the next line.
    while !is_at_eof(current, tokens) {
        if previous(current, tokens).tok_type == TokenType::Semicolon { return current; }

        match peek(current, tokens).tok_type {
            TokenType::If  |
            TokenType::While |
            TokenType::Return => return current,
            _ => ()
        }
        next_token(&mut current, tokens);
    }
    current
}

fn consume_token(token_type: TokenType, message: String, current: usize, tokens: &[Token]) -> (&Token, usize) {
    let mut current = current;
    if check_token(current, tokens, &token_type) {
        return (next_token(&mut current, tokens), current);
    }
    parsing_error(peek(current, tokens), message);
    return (peek(current, tokens), current);
}

fn expression(current: usize, tokens: &[Token]) -> Result<(Box<Expr>, usize), ParserError> {
    return logical_or(current, tokens);
}

fn parsing_error(token: &Token, message: String) {
    if token.tok_type == TokenType::Eof {
        report(token.line_no, String::from(" at end"), message);
    }
    else {
        report(token.line_no, format!(" at '{}'", token.lexeme), message);
    }
}


fn primary(current: usize, tokens: &[Token]) -> Result<(Box<Expr>, usize), ParserError> {
    let mut current = current;
    
    if match_token(&mut current, tokens, &Vec::from([TokenType::False, TokenType::True, TokenType::String, TokenType::Number])) {
        let ret_expr = Box::new(Expr::Literal(LiteralExpr {
            value: Some(previous(current, tokens))
        }));
        return Ok((ret_expr, current));
    }

    if match_token(&mut current, tokens, &Vec::from([TokenType::Identifier])) {
        let identifier = previous(current, tokens);
        if match_token(&mut current, tokens, &Vec::from([TokenType::LeftParen])) {
            // Function call in expression
            let (stmt, current) = function_call_statement(current, tokens, identifier, true)?;
            if let Stmt::FunctionCall(call) = stmt {
                return Ok((Box::new(Expr::Call(CallExpr {
                    call
                })), current));
            }
            else {
                return Err(ParserError);
            }
        }

        let mut var_expr = Box::new(Expr::Variable(VariableExpr {
            name: previous(current, tokens),
        }));

        // Array subscripting
        let mut index_exprs = vec![];
        while match_token(&mut current, tokens, &Vec::from([TokenType::LeftSquareBrace])) {
            let index_expr;
            (index_expr, current) = expression(current, tokens)?;
            index_exprs.push(index_expr);
            (_, current) = consume_token(TokenType::RightSquareBrace, String::from("Expect ']' after array subscript."), current, tokens);
        }

        while let Some(expr) = index_exprs.pop() {
            let addition_expr = Box::new(Expr::Binary(BinaryExpr { 
                left: var_expr, 
                right: expr, 
                operator: Token::generate_token(TokenType::Plus, identifier.line_no)
            }));

            var_expr = Box::new(Expr::Unary(UnaryExpr { 
                operator: Token::generate_token(TokenType::Star, identifier.line_no),
                right: addition_expr 
            }));
        }

        return Ok((var_expr, current));
    }

    // Array initialiser e.g. { 1, 3 + 2, 2 * x }
    let brace_tok = peek(current, tokens);
    if match_token(&mut current, tokens, &Vec::from([TokenType::LeftBrace])) {
        let mut exprs = Vec::new();

        while !match_token(&mut current, tokens, &Vec::from([TokenType::RightBrace])) {
            if !exprs.is_empty() {
                (_, current) = consume_token(TokenType::Comma, String::from("Expect ',' delimiting expressions."), current, tokens);
            }
            let expr;
            (expr, current) = expression(current, tokens)?;
            exprs.push(*expr);
        }

        let size = &exprs.len();
        let ret_expr = Box::new(Expr::ArrayIntialiser(ArrayIntialiserExpr {
            elements: exprs,
            brace_tok,
            step_size: *size,
            var_type: brace_tok
        }));
        return Ok((ret_expr, current));
    }

    if match_token(&mut current, tokens, &Vec::from([TokenType::LeftParen])) {
        let (expr, mut current) = expression(current, tokens)?;
        (_, current) = consume_token(TokenType::RightParen, String::from("Expect ')' after expression."), current, tokens);
        
        let ret_expr = Box::new(Expr::Grouping(GroupingExpr {
            expression: expr,
        }));
        return Ok((ret_expr, current));
    }

    // parsing_error(peek(current, tokens), String::from("Expect expression."));
    Err(ParserError)
}

fn unary(current: usize, tokens: &[Token]) -> Result<(Box<Expr>, usize), ParserError> {
    let mut current = current;
    let current_tok = peek(current, tokens);

    if current_tok.tok_type == TokenType::LeftParen {
        let next_tok = match next(current, tokens) {
            Some(t) => t,
            None => { 
                parsing_error(current_tok, String::from("Expect expression after '('.")); 
                return Err(ParserError)
            }
        };

        if next_tok.tok_type == TokenType::Int || next_tok.tok_type == TokenType::Float {
            let cast_type = next_tok;
            (_, current) = consume_token(TokenType::LeftParen, String::from(""), current, tokens);
            (_, current) = consume_token(next_tok.tok_type, String::from(""), current, tokens);
            (_, current) = consume_token(TokenType::RightParen, String::from("Expect ')' after cast type."), current, tokens);
            let right: Box<Expr>;
            (right, current) = unary(current, tokens)?;
            let expr = Box::new(Expr::Unary(UnaryExpr { operator: cast_type.clone(), right }));
            return Ok((expr, current));
        }
    }

    if match_token(&mut current, tokens, &Vec::from([TokenType::Bang, TokenType::Minus, TokenType::Star, TokenType::Tilde, TokenType::BitwiseAnd, TokenType::Star])) {
        let right: Box<Expr>;
        let operator = previous(current, tokens); 
        (right, current) = unary(current, tokens)?;
        let expr = Box::new(Expr::Unary(UnaryExpr {
            operator: operator.clone(),
            right
        }));
        return Ok((expr, current));
    }

    return primary(current, tokens);
}

fn ast_binary_operation<'a>(current: usize, tokens: &'a [Token], token_types: &Vec<TokenType>, preceding_op: impl Fn(usize, &[Token]) -> Result<(Box<Expr>, usize), ParserError>) -> Result<(Box<Expr<'a>>, usize), ParserError> {
    let (mut expr, mut current) = preceding_op(current, tokens)?;

    while match_token(&mut current, tokens, token_types) {
        let right: Box<Expr>;
        let operator = previous(current, tokens);
        (right, current) = preceding_op(current, tokens)?;
        expr = Box::new(Expr::Binary(BinaryExpr {
            left: expr, 
            right,
            operator: operator.clone()
        }));
    }
    Ok((expr, current))
}

fn factor(current: usize, tokens: &[Token]) -> Result<(Box<Expr>, usize), ParserError> {
    return ast_binary_operation(current, tokens, &Vec::from([TokenType::Star, TokenType::Slash, TokenType::Percent]), unary);
}

fn term(current: usize, tokens: &[Token]) -> Result<(Box<Expr>, usize), ParserError> {
    return ast_binary_operation(current, tokens, &Vec::from([TokenType::Minus, TokenType::Plus]), factor);
}

fn shifting(current: usize, tokens: &[Token]) -> Result<(Box<Expr>, usize), ParserError> {
    return ast_binary_operation(current, tokens, &Vec::from([TokenType::LeftShift, TokenType::RightShift]), term);
}

fn comparison(current: usize, tokens: &[Token]) -> Result<(Box<Expr>, usize), ParserError> {
    return ast_binary_operation(current, tokens, &Vec::from([TokenType::Greater, TokenType::GreaterEqual, TokenType::Less, TokenType::LessEqual]), shifting);
}

fn equality(current: usize, tokens: &[Token]) -> Result<(Box<Expr>, usize), ParserError> {
    return ast_binary_operation(current, tokens, &Vec::from([TokenType::BangEqual, TokenType::EqualEqual]), comparison);
}

fn bitwise_and(current: usize, tokens: &[Token]) -> Result<(Box<Expr>, usize), ParserError> {
    return ast_binary_operation(current, tokens, &Vec::from([TokenType::BitwiseAnd]), equality);
}

fn bitwise_xor(current: usize, tokens: &[Token]) -> Result<(Box<Expr>, usize), ParserError> {
    return ast_binary_operation(current, tokens, &Vec::from([TokenType::BitwiseXor]), bitwise_and);
}

fn bitwise_or(current: usize, tokens: &[Token]) -> Result<(Box<Expr>, usize), ParserError> {
    return ast_binary_operation(current, tokens, &Vec::from([TokenType::BitwiseOr]), bitwise_xor);
}

fn logical_and(current: usize, tokens: &[Token]) -> Result<(Box<Expr>, usize), ParserError> {
    return ast_binary_operation(current, tokens, &Vec::from([TokenType::And]), bitwise_or);
}

fn logical_or(current: usize, tokens: &[Token]) -> Result<(Box<Expr>, usize), ParserError> {
    return ast_binary_operation(current, tokens, &Vec::from([TokenType::Or]), logical_and);
}

fn match_token(current: &mut usize, tokens: &[Token], types: &Vec<TokenType>) -> bool {
    for token_type in types {
        if check_token(*current, tokens, token_type) {
            next_token(current, tokens);
            return true;
        }
    }

    false
}

fn previous(current: usize, tokens: &[Token]) -> &Token {
    match tokens.get(current - 1) {
        Some(token) => token,
        None => panic!("What the hell man!")
    }    
}

fn next(current: usize, tokens: &[Token]) -> Option<&Token> {
    return tokens.get(current + 1);
}

fn next_token<'a>(current: &mut usize, tokens: &'a [Token]) -> &'a Token {
    if !is_at_eof(*current, tokens) { *current += 1; }
    return previous(*current, tokens);
}

fn is_at_eof(current: usize, tokens: &[Token]) -> bool {
    return peek(current, tokens).tok_type == TokenType::Eof;
}

fn check_token(current: usize, tokens: &[Token], token_type: &TokenType) -> bool {
    if is_at_eof(current, tokens) { return false; }
    return peek(current, tokens).tok_type == *token_type;
}

fn peek(current: usize, tokens: &[Token]) -> &Token {
    match tokens.get(current) {
        Some(token) => token,
        None => panic!("What the hell man!")
    }
}