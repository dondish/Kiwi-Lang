use std::vec;

use crate::tokenizer::Token;

pub enum UnaryExpressionType {
    Not,
}

pub enum BinaryExpressionType {
    Add,
    Subtract
}
pub enum ASTNode {
    Literal(Token),
    Identifier(Token),
    UnaryExpression { expression_type: UnaryExpressionType, argument: Box<ASTNode> },
    BinaryExpression { expression_type: BinaryExpressionType, left_argument: Box<ASTNode>, right_argument: Box<ASTNode>},
    AssignExpression { assignee: Box<ASTNode>, value: Box<ASTNode> },
    FunctionCall { function_name: String, function_arguments: Vec<String>},
    CodeBlock { lines: Vec<ASTNode> },
    FunctionDefinition { function_name: String, function_arguments: Vec<String>, function_code: Box<ASTNode>}
}

pub struct AST {
    root_nodes: Vec<ASTNode>
}

struct ASTBuildState<'a> {
    root_nodes: Vec<ASTNode>,
    tokens: &'a Vec<Token>,
    current_index: usize
}

impl <'a> ASTBuildState<'a> {

    fn new(tokens: &Vec<Token>) -> ASTBuildState {
        ASTBuildState { root_nodes: vec![], tokens: &tokens, current_index: 0 }
    }

}

fn build_ast(tokens: Vec<Token>) -> Result<AST, &'static str> {
    let mut build_state = ASTBuildState::new(&tokens);

    while build_state.current_index < tokens.len() {
        match tokens[build_state.current_index] {
            Token::Space | Token::NewLine => build_state.current_index += 1,
            Token::Identifier(_)  => build_state.root_nodes.push(build_statement(&mut build_state)?),
            Token::Define => {
                build_state.current_index += 1;
                build_state.root_nodes.push(build_function(&mut build_state))
            }
            ,
            _ => { println!("Failed parsing at token"); return Err("Failed parsing at token")}
        }
    }

    Ok(AST {root_nodes: build_state.root_nodes})
}

fn build_expression(build_state: &mut ASTBuildState) -> Result<ASTNode, &'static str> {
    build_expression_recursive(build_state, false)
}

fn build_expression_recursive(build_state: &mut ASTBuildState, in_parens: bool) -> Result<ASTNode, &'static str> {
    
}

fn build_non_function_call_expression(build_state: &mut ASTBuildState, in_parens: bool) -> Result<ASTNode, &'static str> {
    let mut expr_stack: Vec<ASTNode> = vec![];
    let mut operator_stack: Vec<Token> = vec![];
    let mut current_token = build_state.tokens[build_state.current_index];
    

    while !is_expression_terminal_token(&current_token) {
        match current_token {
            Token::FloatLiteral(_) | Token::IntLiteral(_) | Token::StringLiteral(_) => expr_stack.push(ASTNode::Literal(current_token)),
            Token::Identifier(_) => expr_stack.push(ASTNode::Identifier(current_token)),
            Token::Plus | Token::Minus | Token::ExclamationMark => {
                while let Some(stack_top) = operator_stack.last() {
                    if get_operator_precedence(stack_top) < get_operator_precedence(&current_token) {
                        break;
                    }
                    operator_stack.pop();
                    if is_binary_operator(&stack_top) {
                        let right_expr = expr_stack.pop();
                        let left_expr = expr_stack.pop();
                        if left_expr.is_none() || right_expr.is_none() {
                            return Err("No expressions for operator");
                        }
                        expr_stack.push(ASTNode::BinaryExpression{ 
                            expression_type: get_binary_expression_type(&stack_top)?, 
                            left_argument: Box::new(left_expr.unwrap()), 
                            right_argument: Box::new(right_expr.unwrap())
                        });
                    } else if is_unary_operator(&stack_top) {
                        let expr = expr_stack.pop();

                        if expr.is_none() {
                            return Err("No expressions for operator");
                        }
                        expr_stack.push(ASTNode::UnaryExpression {
                            expression_type: get_unary_expression_type(&stack_top)?,
                            argument: Box::new(expr.unwrap())
                        });
                    }
                }
            }
            Token::LeftParen => expr_stack.push(build_expression_recursive(build_state, true)?),
            Token::RightParen => {
                if in_parens {
                    build_state.current_index += 1;
                    break;
                }
                return Err("Unexpected )");
            },
            Token::Space => {},
            _ => return Err("Invalid token")
        }
        build_state.current_index += 1;
        current_token = build_state.tokens[build_state.current_index];
    }

    while let Some(stack_top) = operator_stack.pop() {
        if is_binary_operator(&stack_top) {
            let right_expr = expr_stack.pop();
            let left_expr = expr_stack.pop();
            if left_expr.is_none() || right_expr.is_none() {
                return Err("No expressions for operator");
            }
            expr_stack.push(ASTNode::BinaryExpression{ 
                expression_type: get_binary_expression_type(&stack_top)?, 
                left_argument: Box::new(left_expr.unwrap()), 
                right_argument: Box::new(right_expr.unwrap())
            });
        } else if is_unary_operator(&stack_top) {
            let expr = expr_stack.pop();

            if expr.is_none() {
                return Err("No expressions for operator");
            }
            expr_stack.push(ASTNode::UnaryExpression {
                expression_type: get_unary_expression_type(&stack_top)?,
                argument: Box::new(expr.unwrap())
            });
        }
    }



    expr_stack.pop().ok_or_else(|| "No expressions")
}

fn get_operator_precedence(token: &Token) -> u32 {
    match token {
        Token::Plus | Token::Minus => 1,
        Token::ExclamationMark => 5,
        Token::LeftParen | Token::Identifier(_) => 100,
        _ => 0
    }
}

fn is_binary_operator(token: &Token) -> bool {
    match token {
        Token::Plus | Token::Minus => true,
        _ => false
    }
}

fn is_unary_operator(token: &Token) -> bool {
    match token {
        Token::ExclamationMark => true,
        _ => false
    }
}

fn get_binary_expression_type(token: &Token) -> Result<BinaryExpressionType, &'static str> {
    match token {
        Token::Plus => Ok(BinaryExpressionType::Add),
        Token::Minus => Ok(BinaryExpressionType::Subtract),
        _ => Err("Invalid token")
    }
}

fn get_unary_expression_type(token: &Token) -> Result<UnaryExpressionType, &'static str> {
    match token {
        Token::ExclamationMark => Ok(UnaryExpressionType::Not),
        _ => Err("Invalid token")
    }
}

fn is_expression_terminal_token(token: &Token) -> bool {
    match token {
        Token::Eof | Token::NewLine | Token::Unknown | Token::RightCurlyBracket => true,
        _ => false
    }
}

fn build_statement(build_state: &mut ASTBuildState) -> Result<ASTNode, &'static str> {
    skip_over_whitespaces(build_state);
    let left_side = vec![];
    let i = build_state.current_index;

    while i < build_state.tokens.len() {
        let token = build_state.tokens[i];
        match token {
            Token::Define | Token::LeftCurlyBracket | Token::RightCurlyBracket | Token::RightParen | Token::Unknown => { println!("Error"); break; },
            Token::NewLine => break,
            Token::LeftParen => b
            _ => i += 1
        }
    }

    if let 
}

fn build_function(build_state: &mut ASTBuildState) -> ASTNode {
    
}

fn skip_over_whitespaces(build_state: &mut ASTBuildState) {
    while build_state.current_index < build_state.tokens.len() {
        if let Token::NewLine | Token::Space = build_state.tokens[build_state.current_index] {
            build_state.current_index += 1;
        } else {
            return;
        }
    }
}