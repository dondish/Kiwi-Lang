use std::vec;

use crate::tokenizer::Token;

/// Unary Expression Type like ! and ~
#[derive(Debug, PartialEq)]
pub enum UnaryExpressionType {
    Not, // !
}

impl <'a> TryFrom<&'a Token> for UnaryExpressionType {
    type Error = &'static str;

    fn try_from(token: &'a Token) -> Result<Self, Self::Error> {
        match token {
            Token::ExclamationMark => Ok(UnaryExpressionType::Not),
            _ => Err("Invalid token")
        }
    }
}

/// Binary Expression Type like + or -
#[derive(Debug, PartialEq)]
pub enum BinaryExpressionType {
    Add, // +
    Subtract // -
}

impl <'a> TryFrom<&'a Token> for BinaryExpressionType {
    type Error = &'static str;

    fn try_from(token: &'a Token) -> Result<Self, Self::Error> {
        match token {
            Token::Plus => Ok(BinaryExpressionType::Add),
            Token::Minus => Ok(BinaryExpressionType::Subtract),
            _ => Err("Invalid token")
        }
    }
}

/// An enumeration of nodes in the AST
#[derive(Debug, PartialEq)]
pub enum ASTNode {
    Literal(Token), // Any type of literal
    Identifier(Token), // An identifier
    UnaryExpression { expression_type: UnaryExpressionType, argument: Box<ASTNode> }, // A unary expression
    BinaryExpression { expression_type: BinaryExpressionType, left_argument: Box<ASTNode>, right_argument: Box<ASTNode>}, // A binary expression
    AssignExpression { assignee: Box<ASTNode>, value: Box<ASTNode> }, // Assignment expression
    FunctionCall { function: Box<ASTNode>, function_arguments: Vec<Box<ASTNode>>}, // Function call
    CodeBlock { lines: Vec<Box<ASTNode>> }, // Multiple Statements
    FunctionDefinition { function_name: Box<ASTNode>, function_arguments: Vec<Box<ASTNode>>, function_code: Box<ASTNode>}
}

/// An Abstract Syntax Tree
pub struct AST {
    root_nodes: Vec<Box<ASTNode>>
}

/// AST Build State
struct ASTBuildState<'a> {
    root_nodes: Vec<Box<ASTNode>>,
    tokens: &'a Vec<Token>,
    current_index: usize
}

impl <'a> ASTBuildState<'a> {

    fn new(tokens: &Vec<Token>) -> ASTBuildState {
        ASTBuildState { root_nodes: vec![], tokens: &tokens, current_index: 0 }
    }

}

/// Builds the ast
pub fn build_ast(tokens: &Vec<Token>) -> Result<AST, &'static str> {
    let mut build_state = ASTBuildState::new(tokens);

    while build_state.current_index < tokens.len() {
        match tokens[build_state.current_index] {
            Token::Space | Token::NewLine => build_state.current_index += 1,
            Token::Identifier(_)  => {
                let statement = build_statement(&mut build_state)?;
                build_state.root_nodes.push(statement)
            },
            Token::Define => {
                build_state.current_index += 1;
                let function_definition = build_function(&mut build_state)?;
                build_state.root_nodes.push(function_definition);
            }
            ,
            _ => { println!("Failed parsing at token"); return Err("Failed parsing at token")}
        }
    }

    Ok(AST {root_nodes: build_state.root_nodes})
}

/// Builds an expression
fn build_expression(build_state: &mut ASTBuildState) -> Result<Box<ASTNode>, &'static str> {
    build_expression_recursive(build_state, false)
}

/// Builds an expression which may be a function call
fn build_expression_recursive(build_state: &mut ASTBuildState, in_parens: bool) -> Result<Box<ASTNode>, &'static str> {
    let mut expression_fragments: Vec<Vec<Token>> = vec![vec![]];
    let mut was_last_operator = true;
    let mut current_token = &build_state.tokens[build_state.current_index];

    while !is_expression_terminal_token(current_token) {
        match current_token {
            Token::LeftParen => { // handle nested parens, just add the tokens to the correct expression fragment
                if !was_last_operator { // If the last token was not an operator, 
                    expression_fragments.push(vec![]);
                }
                was_last_operator = false; // Operator cannot be a result of the parenthesis
                
                let mut indentation = 0;
    
                while !is_expression_terminal_token(&current_token) { // handle indentations
                    expression_fragments.last_mut().unwrap().push(current_token.to_owned());
                    match current_token {
                        Token::LeftParen => indentation += 1,
                        Token::RightParen => {
                            indentation -= 1;
                            if indentation == 0 {
                                break;
                            }
                        }
                        _ => {}
                    }
                    build_state.current_index += 1;
                    current_token = &build_state.tokens[build_state.current_index];
                }
            }
            Token::RightParen => {
                if in_parens {
                    break;
                } else {
                    return Err("Unexpected )")
                }
            },
            _ => {}
        }
        if is_operator(&current_token) {
            was_last_operator = true;
            expression_fragments.last_mut().unwrap().push(current_token.to_owned());
        } else if was_last_operator {
            match current_token {
                Token::Space => {}
                _ => was_last_operator = false
            }
            expression_fragments.last_mut().unwrap().push(current_token.to_owned());
        } else {
            expression_fragments.push(vec![current_token.to_owned()]);
        }
        build_state.current_index += 1;
        
        if build_state.current_index == build_state.tokens.len() {
            break;
        }
        
        current_token = &build_state.tokens[build_state.current_index];
    }

    if expression_fragments.len() > 1 {
        Ok(Box::new(ASTNode::FunctionCall {
            function: build_non_function_call_expression(&mut ASTBuildState::new(&expression_fragments[0]), in_parens)?,
            function_arguments: expression_fragments
                .iter()
                .skip(1)
                .map(|tokens| build_non_function_call_expression(&mut ASTBuildState::new(tokens), in_parens))
                .collect::<Result<Vec<Box<ASTNode>>, &'static str>>()?
        }))
    } else {
        build_non_function_call_expression(&mut ASTBuildState::new(&expression_fragments[0]), in_parens)
    }

}

/// Builds non expressions which are not function calls (can include one in parens)
/// Uses the shunting yard algorithm
fn build_non_function_call_expression(build_state: &mut ASTBuildState, in_parens: bool) -> Result<Box<ASTNode>, &'static str> {
    let mut expr_stack: Vec<Box<ASTNode>> = vec![];
    let mut operator_stack: Vec<Box<Token>> = vec![];
    let mut current_token = &build_state.tokens[build_state.current_index];
    

    while !is_expression_terminal_token(&current_token) {
        match current_token {
            Token::FloatLiteral(_) | Token::IntLiteral(_) | Token::StringLiteral(_) => expr_stack.push(Box::new(ASTNode::Literal(current_token.to_owned()))),
            Token::Identifier(_) => expr_stack.push(Box::new(ASTNode::Identifier(current_token.to_owned()))),
            Token::Plus | Token::Minus | Token::ExclamationMark => {
                shunting_yard_look_for_lower_precedence(current_token, &mut operator_stack, &mut expr_stack)?;
                operator_stack.push(Box::new(current_token.to_owned()));
            }
            Token::LeftParen => {
                build_state.current_index += 1;
                expr_stack.push(build_expression_recursive(build_state, true)?)
            },
            Token::RightParen => {
                if in_parens {
                    break;
                }
                return Err("Unexpected )");
            },
            Token::Space => {},
            _ => return Err("Invalid token")
        }
        build_state.current_index += 1;

        if build_state.current_index == build_state.tokens.len() {
            break
        }

        current_token = &build_state.tokens[build_state.current_index];
    }

    while let Some(stack_top) = operator_stack.pop() {
        push_operation_to_expr_stack(stack_top.as_ref(), &mut expr_stack)?;
    }

    expr_stack.pop().ok_or("No expressions")
}

fn shunting_yard_look_for_lower_precedence(current_token: &Token, operator_stack: &mut Vec<Box<Token>>, expr_stack: &mut Vec<Box<ASTNode>>) -> Result<(), &'static str>{
    while let Some(stack_top) = operator_stack.pop() {
        if get_operator_precedence(stack_top.as_ref()) < get_operator_precedence(current_token) {
            operator_stack.push(stack_top);
            break;
        }

        push_operation_to_expr_stack(stack_top.as_ref(), expr_stack)?;
    }
    Ok(())
}

fn push_operation_to_expr_stack(operator: &Token, expr_stack: &mut Vec<Box<ASTNode>>) -> Result<(), &'static str> {
    if BinaryExpressionType::try_from(operator).is_ok() {
        let right_expr = expr_stack.pop().ok_or("No expressions for operator")?;
        let left_expr = expr_stack.pop().ok_or("No expressions for operator")?;

        expr_stack.push(Box::new(ASTNode::BinaryExpression{ 
            expression_type: operator.try_into()?, 
            left_argument: left_expr, 
            right_argument: right_expr
        }));
    } else if UnaryExpressionType::try_from(operator).is_ok() {
        let expr = expr_stack.pop().ok_or("No expressions for operator")?;

        expr_stack.push(Box::new(ASTNode::UnaryExpression {
            expression_type: operator.try_into()?,
            argument: expr
        }));
    }
    Ok(())
}

/// Returns the precedence of an operator
fn get_operator_precedence(token: &Token) -> u32 {
    match token {
        Token::Plus | Token::Minus => 1,
        Token::ExclamationMark => 5,
        Token::LeftParen | Token::Identifier(_) => 100,
        _ => 0
    }
}

/// Returns whether a token represents an operator
fn is_operator(token: &Token) -> bool {
    BinaryExpressionType::try_from(token).is_ok() || UnaryExpressionType::try_from(token).is_ok()
}

/// Returns whether the token terminates the expression
fn is_expression_terminal_token(token: &Token) -> bool {
    match token {
        Token::Eof | Token::NewLine | Token::Unknown | Token::RightCurlyBracket => true,
        _ => false
    }
}

/// Builds an AST statement 
fn build_statement(_build_state: &mut ASTBuildState) -> Result<Box<ASTNode>, &'static str> {
    Err("Unimplemented")
}

/// Builds an AST function
fn build_function(_build_state: &mut ASTBuildState) -> Result<Box<ASTNode>, &'static str> {
    Err("Unimplemented")
}

/// Increments the build state's index until it no longer points on a white-space token
fn skip_over_whitespaces(build_state: &mut ASTBuildState) {
    while build_state.current_index < build_state.tokens.len() {
        if let Token::NewLine | Token::Space = build_state.tokens[build_state.current_index] {
            build_state.current_index += 1;
        } else {
            return;
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{tokenizer::Token, parser::{UnaryExpressionType, BinaryExpressionType}};

    use super::{build_expression, ASTBuildState, ASTNode};

    fn get_expression_from_tokens(tokens: &Vec<Token>) -> Result<Box<ASTNode>, &'static str> {
        let mut build_state = ASTBuildState::new(&tokens);
        build_expression(&mut build_state)
    }


    #[test]
    /// Tests basic binary operator: a + b
    fn basic_binary_operator() {
        let expression = get_expression_from_tokens(&vec![Token::Identifier("a".to_string()), Token::Plus, Token::Identifier("b".to_string())]); // a + b
        
        assert_eq!(
            expression, 
            Ok(
                Box::new(
                    ASTNode::BinaryExpression {
                        expression_type: BinaryExpressionType::Add,
                        left_argument: Box::new(
                            ASTNode::Identifier(
                                Token::Identifier("a".to_string())
                            )
                        ),
                        right_argument: Box::new(
                            ASTNode::Identifier(
                                Token::Identifier("b".to_string())
                            )
                        )
                    }
                )
            ),
            "Failed basic binary operator: a + b"
        );
    }

    #[test]
    /// Tests basic function call: print "Hello World!"
    fn basic_function_call() {
        let expression = get_expression_from_tokens(&vec![Token::Identifier("print".to_string()), Token::StringLiteral("Hello World!".to_string())]);

        assert_eq!(
            expression,
            Ok(
                Box::new(
                    ASTNode::FunctionCall {
                        function: Box::new(
                            ASTNode::Identifier(
                                Token::Identifier("print".to_string())
                            )
                        ),
                        function_arguments: vec![
                            Box::new(
                                ASTNode::Literal(
                                    Token::StringLiteral("Hello World!".to_string())
                                )
                            )
                        ]
                    }
                )
            ),
            "Failed basic function call: print \"Hello World!\""
        )
    }

    #[test]
    /// Test basic unary operator: !1
    fn basic_unary_operator() {
        let expression = get_expression_from_tokens(&vec![Token::ExclamationMark, Token::IntLiteral(1)]);

        assert_eq!(
            expression,
            Ok(
                Box::new(
                    ASTNode::UnaryExpression {
                        expression_type: UnaryExpressionType::Not,
                        argument: Box::new(
                            ASTNode::Literal(
                                Token::IntLiteral(1)
                            )
                        )
                    }
                )
            ),
            "Failed basic unary operator: !1"
        )
    }

    #[test]
    /// Test basic nested expression: a + b - c
    fn basic_nested_expression() {
        let expression = get_expression_from_tokens(&vec![Token::Identifier("a".to_string()), Token::Plus, Token::Identifier("b".to_string()), Token::Minus, Token::Identifier("c".to_string())]);
        
        assert_eq!(
            expression,
            Ok(
                Box::new(
                    ASTNode::BinaryExpression {
                        expression_type: BinaryExpressionType::Subtract,
                        left_argument: Box::new(
                            ASTNode::BinaryExpression {
                                expression_type: BinaryExpressionType::Add,
                                left_argument: Box::new(
                                    ASTNode::Identifier(
                                        Token::Identifier(
                                            "a".to_string()
                                        )
                                    )
                                ),
                                right_argument: Box::new(
                                    ASTNode::Identifier(
                                        Token::Identifier("b".to_string())
                                    ),
                                )
                            }
                        ),
                        right_argument: Box::new(
                            ASTNode::Identifier(
                                Token::Identifier(
                                    "c".to_string()
                                ))
                        )
                    }
                )
            ),
            "Failed basic nested expression: a + b - c"
        )
    }

    #[test]
    /// Basic operator precedence: a + !b
    fn basic_operator_precedence() {
        let expression = get_expression_from_tokens(&vec![Token::Identifier("a".to_string()), Token::Plus, Token::ExclamationMark, Token::Identifier("b".to_string())]);

        assert_eq!(
            expression,
            Ok(
                Box::new(
                    ASTNode::BinaryExpression {
                        expression_type: BinaryExpressionType::Add,
                        left_argument: Box::new(
                            ASTNode::Identifier(
                                Token::Identifier("a".to_string())
                            )
                        ),
                        right_argument: Box::new(
                            ASTNode::UnaryExpression {
                                expression_type: UnaryExpressionType::Not,
                                argument: Box::new(
                                    ASTNode::Identifier(
                                        Token::Identifier("b".to_string())
                                    )
                                )
                            }
                        )
                    }
                )
            ),
            "Failed basic operator precedence: a + !b"
        )
    }
}