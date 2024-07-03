use crate::{error_handler::report, compiler::scanner::{Token, TokenType}};
struct ParserError;

#[derive(Clone)]
pub struct BinaryExpr<'a> {
    pub(super) left: Box<Expr<'a>>,
    pub(super) right: Box<Expr<'a>>,
    pub(super) operator: &'a Token,
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
    pub(super) operator: &'a Token,
    pub(super) right: Box<Expr<'a>>,
}

#[derive(Clone)]
pub struct VariableExpr<'a> {
    pub(super) name: &'a Token,
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
    StoredValueExpr(StoredValueExpr)
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
}

pub struct IfStmt<'a> {
    pub(super) conditions: Vec<Expr<'a>>,
    pub(super) branches: Vec<Stmt<'a>>
}

pub struct ReturnStmt<'a> {
    pub(super) value: Box<Expr<'a>>
}

pub struct VarStmt<'a> {
    pub(super) var_name: &'a Token,
    pub(super) var_type: &'a Token,
    pub(super) initialiser: Option<Box<Expr<'a>>>
}

pub struct AssignStmt<'a> {
    pub(super) var_name: &'a Token,
    pub(super) binding: Box<Expr<'a>>
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
    pub(super) params: Vec<Expr<'a>>
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

pub fn pretty_print_stmt(stmt: &Stmt, depth: usize) -> String {
    match stmt {
        Stmt::Function(s) => {
            let mut str = String::from(format!("{} {}(", s.ret_type.lexeme, s.name.lexeme));
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
            return str;
        },
        Stmt::Variable(s) => {
            let mut str = String::from(format!("{} {}", s.var_type.lexeme, s.var_name.lexeme));
            match s.initialiser.clone() {
                Some(init) => str.push_str(&format!(" = {};\n", ast_pretty_printer(init))),
                None => str.push_str(";\n"),
            }
            return str;
        },
        Stmt::Expression(s) => {
            return ast_pretty_printer(s.expression.clone())
        }
        Stmt::While(s) => {
            let mut str = String::from(format!("while ({})", ast_pretty_printer(s.condition.clone())));
            str.push_str(&pretty_print_stmt(&s.body, depth));
            return str;
        }
        Stmt::Block(s) => {
            let depth = depth + 1;
            let mut str = String::from("");
            str.push_str(" {\n");
            for stmt in s.statements.iter() {
                str.push_str(&format!("{}{}", indent_depth(depth), pretty_print_stmt(&stmt, depth)));
            }
            str.push_str(&format!("{}}}\n", indent_depth(depth - 1)));
            return str;
        }
        Stmt::For(s) => {
            let variant_str = { 
                match &s.variant {
                    Some(v) => pretty_print_stmt(&v, depth),
                    None => String::from("  "),
                }
            };

            let incrementer_str = {
                match &s.incrementer {
                    Some(i) => pretty_print_stmt(&i, depth),
                    None => String::from("  "),
                }
            };
            
            // Remove that last two characters ';' and '\n'
            let mut str = String::from(format!("for ({}; {}; {})", 
                &variant_str[..(variant_str.len() - 2)], 
                ast_pretty_printer(s.condition.clone()), 
                &incrementer_str[..(incrementer_str.len() - 2)]));

            str.push_str(&pretty_print_stmt(&s.body, depth));
            return str;
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
                            Some(c) => ast_pretty_printer(Box::new(c.clone())),
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
            return str;
        },
        Stmt::Assign(s) => String::from(format!("{} = {};\n", s.var_name.lexeme, ast_pretty_printer(s.binding.clone()))),
        Stmt::Return(s) => String::from(format!("return {};\n", ast_pretty_printer(s.value.clone()))),
        Stmt::FunctionCall(s) => {
            let mut str = format!("{}(", s.name.lexeme);
            for (i, expr) in s.params.iter().enumerate() {
                str.push_str(&ast_pretty_printer(Box::new(expr.clone())));
                if i != s.params.len() - 1 {
                    str.push_str(", ");
                }
            } 
            str.push_str(");\n");
            return str;
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
    return str;
}

fn parenthesize_expr(expr: Box<Expr>) -> String {
    match *expr {
        Expr::Binary(e) => parenthesize(e.operator.lexeme.clone(), Vec::from([e.left, e.right])),
        Expr::Grouping(e) => parenthesize(String::from("group"), Vec::from([e.expression])),
        Expr::Literal(e) => {
            match e.value {
                Some(v) => String::from(v.lexeme.clone()),
                None => String::from("")
            }
        },
        Expr::Unary(e) => parenthesize(e.operator.lexeme.clone(), Vec::from([e.right])),
        Expr::Variable(e) => parenthesize(e.name.lexeme.clone(), Vec::from([])),
        Expr::Call(e) => {
            let call_stmt = Stmt::FunctionCall(e.call.clone());
            pretty_print_stmt(&call_stmt, 0) 
        },
        Expr::StoredValueExpr(sto) => format!("{}", sto.value)
    }
} 

fn parenthesize(name: String, exprs: Vec<Box<Expr>>) -> String {
    let mut result = String::from("(");
    result.push_str(&name);
    for expr in exprs {
        result.push_str(" ");
        result.push_str(&parenthesize_expr(expr));
    }
    result.push_str(")");
    return result;
}

pub fn ast_pretty_printer(expr: Box<Expr>) -> String {
    return parenthesize_expr(expr)
}

pub fn parse(tokens: &Vec<Token>) -> Vec<Stmt> {
    let mut statements: Vec<Stmt> = vec![];
    let mut current = 0;

    while !is_at_eof(current, tokens) {
        let stmt_res = declaration_statement(current, tokens, 0, false);
        match stmt_res {
            Ok((stmt, c)) => {
                current = c;
                println!("{}", pretty_print_stmt(&stmt, 0));
                statements.push(stmt);
            }
            Err(_) => panic!("Internal compiler error.")
        }
    } 

    return statements;
}

fn block_statement(current: usize, tokens: &Vec<Token>, depth: usize) -> Result<(BlockStmt, usize), ParserError> {
    let mut current = current;

    // Start of block
    if !(match_token(&mut current, tokens, &Vec::from([TokenType::LeftBrace]))) {
        parsing_error(peek(current, tokens), String::from("Expected '{'."));
        return Err(ParserError);
    }

    let mut stmts: Vec<Stmt> = Vec::new();
    while !(match_token(&mut current, tokens, &Vec::from([TokenType::RightBrace]))) {
        let stmt_res = declaration_statement(current, tokens, depth + 1, false);
        match stmt_res {
            Ok((stmt, c)) => { 
                stmts.push(stmt); 
                current = c;
            },
            Err(_) => return Err(ParserError)
        }
    }
    return Ok((BlockStmt { statements: stmts }, current));
}

fn for_statement(current: usize, tokens: &Vec<Token>) -> Result<(Stmt, usize), ParserError> {
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
        let initialisation = declaration_statement(current, tokens, 2, true);
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
        condition = Box::new(Expr::StoredValueExpr(StoredValueExpr { value: 1 } ));
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
        match declaration_statement(current, tokens, 2, true) {
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
    match block_statement(current, tokens, 2) {
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

fn while_statement(current: usize, tokens: &Vec<Token>) -> Result<(Stmt, usize), ParserError> {
    let mut current = current;
    (_, current) = consume_token(TokenType::LeftParen, String::from("Expect '(' after while."), current, tokens);

    let condition;
    match expression(current, tokens) {
        Ok((cond, c)) => (condition, current) = (cond, c),
        Err(_) => return Err(ParserError)
    }

    (_, current) = consume_token(TokenType::RightParen, String::from("Expect ')' after condition."), current, tokens);

    let body;
    match block_statement(current, tokens, 2) {
        Ok((b, c)) => (body, current) = (b, c),
        Err(_) => return Err(ParserError)
    }

    return Ok((Stmt::While(WhileStmt {
        condition,
        body: Box::new(Stmt::Block(body)),
    }), current));
}

fn if_statement(current: usize, tokens: &Vec<Token>) -> Result<(Stmt, usize), ParserError> {
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
            
            match block_statement(current, tokens, 2) {
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
        match block_statement(current, tokens, 2) {
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

fn function_call_statement<'a>(current: usize, tokens: &'a Vec<Token>, identifier: &'a Token, inline: bool) -> Result<(Stmt<'a>, usize), ParserError> {
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

fn generate_statement(current: usize, tokens: &Vec<Token>, inline_statement: bool) -> Result<(Stmt, usize), ParserError> {
    let mut current = current;
    if match_token(&mut current, tokens, &Vec::from([TokenType::While])) { return while_statement(current, tokens); }
    else if match_token(&mut current, tokens, &Vec::from([TokenType::For])) { return for_statement(current, tokens); }
    else if match_token(&mut current, tokens, &Vec::from([TokenType::If])) { return if_statement(current, tokens); }
    else if match_token(&mut current, tokens, &Vec::from([TokenType::Identifier])) {
        let identifier = previous(current, tokens);

        if match_token(&mut current, tokens, &Vec::from([TokenType::Equal])) {
            if let Ok((binding, current)) = expression_statement(current, tokens) {
                let mut current = current;
                if !inline_statement {
                    (_, current) = consume_token(TokenType::Semicolon, String::from("Expect ';' after assignment."), current, tokens);
                }
                return Ok((Stmt::Assign(AssignStmt {
                    var_name: identifier,
                    binding
                }), current));
            }
            else {
                return Err(ParserError);
            }
        }
        else if match_token(&mut current, tokens, &Vec::from([TokenType::LeftParen])) { return function_call_statement(current, tokens, identifier, false); }
    }
    else if match_token(&mut current, tokens, &Vec::from([TokenType::Return])) {
        if let Ok((binding, current)) = expression_statement(current, tokens) {
            let mut current = current;
            (_, current) = consume_token(TokenType::Semicolon, String::from("Expect ';' after return expression."), current, tokens);
            return Ok((Stmt::Return(ReturnStmt {
                value: binding
            }), current))
        }
    }

    println!("{}", peek(current, tokens).lexeme);
    return Err(ParserError);
}

fn expression_statement(current: usize, tokens: &Vec<Token>) -> Result<(Box<Expr>, usize), ParserError> {
    return expression(current, tokens); 
}

fn declaration_statement(current: usize, tokens: &Vec<Token>, depth: usize, inline_statement: bool) -> Result<(Stmt, usize), ParserError> {
    let mut current = current;
    if match_token(&mut current, tokens, &Vec::from([TokenType::Int, TokenType::Float, TokenType::Double, TokenType::Char])) {
        let var_dec_opt = variable_declaration(current, tokens, inline_statement, depth);
        match var_dec_opt {
            Ok((stmt, c)) => {
                return Ok((stmt, c)); 
            },
            Err(_) => {
                current = error_synchronise(current, tokens);
                return Ok((Stmt::Expression(ExprStmt {
                    expression: Box::new(Expr::StoredValueExpr(StoredValueExpr { value: 1 }))
                }), current));
            }
        }
    }
    else {
        let stmt_opt = generate_statement(current, tokens, inline_statement);
        match stmt_opt {
            Ok((s, c)) => return Ok((s, c)),
            Err(_) => {
                current = error_synchronise(current, tokens);
                return Ok((Stmt::Expression(ExprStmt {
                    expression: Box::new(Expr::StoredValueExpr(StoredValueExpr { value: 1 }))
                }
                ), current));
            }
        }
    }
}

// Depth argument prevents nested functions
fn function_declaration<'a>(current: usize, tokens: &'a Vec<Token>, depth: usize, func_name: &'a Token, ret_type: &'a Token) -> Result<(Stmt<'a>, usize), ParserError> {
    let mut current = current;

    if depth > 1 {
        parsing_error(&peek(current, tokens), String::from("Nested functions are not allowed."));
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

    let block_stmts = block_statement(current, tokens, depth);
    match block_stmts {
        Ok((stmts, c)) => {
            Ok((Stmt::Function(FuncStmt {
                name: func_name,
                params: args,
                body: Box::new(Stmt::Block(stmts)),
                ret_type,
            }), c))
        },
        Err(_) => Err(ParserError)
    }
}

// TODO: This function is a bit of a state - Need to figure out what i'm actually doing with the Result<> and Option<>.
fn variable_declaration<'a>(current: usize, tokens: &'a Vec<Token>, is_func_arg: bool, depth: usize) -> Result<(Stmt, usize), ParserError> {
    let mut current = current;
    let var_type = previous(current, tokens);
    let var_name;
    (var_name, current) = consume_token(TokenType::Identifier, String::from("Expect variable name."), current, tokens);


    let initialiser;

    if match_token(&mut current, tokens, &Vec::from([TokenType::Equal])) {
        let initialiser_result = expression_statement(current, tokens);
        match initialiser_result {
            Ok((init, c)) => { 
                (initialiser, current) = (Some(init), c);
            }
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

    return Ok((Stmt::Variable(VarStmt {
        var_name,
        var_type,
        initialiser,
    }), current));

}

pub fn parse_expr(tokens: &Vec<Token>) -> Option<Box<Expr>> {
    let current = 0;
    let expr_result = expression(current, tokens);
    match expr_result {
        Ok((expr, _)) => Some(expr),
        Err(_) => { 
            println!("{}", String::from("Error occured during parsing"));
            None
        }
    }
}

fn error_synchronise(current: usize, tokens: &Vec<Token>) -> usize {
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
    return current;
}

fn consume_token(token_type: TokenType, message: String, current: usize, tokens: &Vec<Token>) -> (&Token, usize) {
    let mut current = current;
    if check_token(current, tokens, &token_type) {
        return (next_token(&mut current, tokens), current);
    }
    parsing_error(peek(current, tokens), message);
    return (peek(current, tokens), current);
}

fn expression(current: usize, tokens: &Vec<Token>) -> Result<(Box<Expr>, usize), ParserError> {
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

fn primary(current: usize, tokens: &Vec<Token>) -> Result<(Box<Expr>, usize), ParserError> {
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
        let ret_expr = Box::new(Expr::Variable(VariableExpr {
            name: previous(current, tokens)
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
    return Err(ParserError);
}

fn unary(current: usize, tokens: &Vec<Token>) -> Result<(Box<Expr>, usize), ParserError> {
    let mut current = current;
    while match_token(&mut current, tokens, &Vec::from([TokenType::Bang, TokenType::Minus, TokenType::BitwiseBang])) {
        let right: Box<Expr>;
        let operator = previous(current, tokens);
        (right, current) = unary(current, tokens)?;
        let expr = Box::new(Expr::Unary(UnaryExpr {
            operator,
            right
        }));
        return Ok((expr, current));
    }
    return Ok(primary(current, tokens)?);
}

fn ast_binary_operation<'a>(current: usize, tokens: &'a Vec<Token>, token_types: &Vec<TokenType>, preceding_op: impl Fn(usize, &Vec<Token>) -> Result<(Box<Expr>, usize), ParserError>) -> Result<(Box<Expr<'a>>, usize), ParserError> {
    let (mut expr, mut current) = preceding_op(current, tokens)?;

    while match_token(&mut current, tokens, token_types) {
        let right: Box<Expr>;
        let operator = previous(current, tokens);
        (right, current) = preceding_op(current, tokens)?;
        expr = Box::new(Expr::Binary(BinaryExpr {
            left: expr, 
            right,
            operator
        }));
    }
    return Ok((expr, current));
}

fn factor(current: usize, tokens: &Vec<Token>) -> Result<(Box<Expr>, usize), ParserError> {
    return ast_binary_operation(current, tokens, &Vec::from([TokenType::Star, TokenType::Slash]), unary);
}

fn term(current: usize, tokens: &Vec<Token>) -> Result<(Box<Expr>, usize), ParserError> {
    return ast_binary_operation(current, tokens, &Vec::from([TokenType::Minus, TokenType::Plus]), factor);
}

fn shifting(current: usize, tokens: &Vec<Token>) -> Result<(Box<Expr>, usize), ParserError> {
    return ast_binary_operation(current, tokens, &Vec::from([TokenType::LeftShift, TokenType::RightShift]), term);
}

fn comparison(current: usize, tokens: &Vec<Token>) -> Result<(Box<Expr>, usize), ParserError> {
    return ast_binary_operation(current, tokens, &Vec::from([TokenType::Greater, TokenType::GreaterEqual, TokenType::Less, TokenType::LessEqual]), shifting);
}

fn equality(current: usize, tokens: &Vec<Token>) -> Result<(Box<Expr>, usize), ParserError> {
    return ast_binary_operation(current, tokens, &Vec::from([TokenType::BangEqual, TokenType::EqualEqual]), comparison);
}

fn bitwise_and(current: usize, tokens: &Vec<Token>) -> Result<(Box<Expr>, usize), ParserError> {
    return ast_binary_operation(current, tokens, &Vec::from([TokenType::BitwiseAnd]), equality);
}

fn bitwise_xor(current: usize, tokens: &Vec<Token>) -> Result<(Box<Expr>, usize), ParserError> {
    return ast_binary_operation(current, tokens, &Vec::from([TokenType::BitwiseXor]), bitwise_and);
}

fn bitwise_or(current: usize, tokens: &Vec<Token>) -> Result<(Box<Expr>, usize), ParserError> {
    return ast_binary_operation(current, tokens, &Vec::from([TokenType::BitwiseOr]), bitwise_xor);
}

fn logical_and(current: usize, tokens: &Vec<Token>) -> Result<(Box<Expr>, usize), ParserError> {
    return ast_binary_operation(current, tokens, &Vec::from([TokenType::And]), bitwise_or);
}

fn logical_or(current: usize, tokens: &Vec<Token>) -> Result<(Box<Expr>, usize), ParserError> {
    return ast_binary_operation(current, tokens, &Vec::from([TokenType::Or]), logical_and);
}

fn match_token(current: &mut usize, tokens: &Vec<Token>, types: &Vec<TokenType>) -> bool {
    for token_type in types {
        if check_token(*current, tokens, token_type) {
            next_token(current, tokens);
            return true;
        }
    }

    return false;
}

fn previous(current: usize, tokens: &Vec<Token>) -> &Token {
    match tokens.get(current - 1) {
        Some(token) => token,
        None => panic!("What the hell man!")
    }    
}

fn next_token<'a>(current: &mut usize, tokens: &'a Vec<Token>) -> &'a Token {
    if !is_at_eof(*current, tokens) { *current += 1; }
    return previous(*current, tokens);
}

fn is_at_eof(current: usize, tokens: &Vec<Token>) -> bool {
    return peek(current, tokens).tok_type == TokenType::Eof;
}

fn check_token(current: usize, tokens: &Vec<Token>, token_type: &TokenType) -> bool {
    if is_at_eof(current, tokens) { return false; }
    return peek(current, tokens).tok_type == *token_type;
}

fn peek<'a>(current: usize, tokens: &'a Vec<Token>) -> &'a Token {
    match tokens.get(current) {
        Some(token) => token,
        None => panic!("What the hell man!")
    }
}