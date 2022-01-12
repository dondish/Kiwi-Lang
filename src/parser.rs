use std::vec;

use crate::tokenizer::Token;

#[derive(Debug, PartialEq)]
pub enum Associativity {
    LeftToRight,
    RightToLeft
}

/// Unary Expression Type like ! and ~
#[derive(Debug, PartialEq, Clone)]
pub enum UnaryExpressionType {
    Not, // !
}

impl <'a> TryFrom<&'a Token> for UnaryExpressionType {
    type Error = ParserError;

    fn try_from(token: &'a Token) -> Result<Self, Self::Error> {
        match token {
            Token::ExclamationMark => Ok(UnaryExpressionType::Not),
            _ =>  Err(ParserError{
                kind: ParserErrorKind::InvalidUnaryOperatorToken,
                token: token.to_owned()
            })
        }
    }
}

/// Binary Expression Type like + or -
#[derive(Debug, PartialEq, Clone)]
pub enum BinaryExpressionType {
    Add, // +
    Subtract, // -
    Assign // =
}

impl BinaryExpressionType {
    pub fn get_associativity(self: &Self) -> Associativity {
        match self {
            BinaryExpressionType::Add => Associativity::LeftToRight,
            BinaryExpressionType::Subtract => Associativity::LeftToRight,
            BinaryExpressionType::Assign => Associativity::RightToLeft
        }
    }
}

impl <'a> TryFrom<&'a Token> for BinaryExpressionType {
    type Error = ParserError;

    fn try_from(token: &'a Token) -> Result<Self, Self::Error> {
        match token {
            Token::Plus => Ok(BinaryExpressionType::Add),
            Token::Minus => Ok(BinaryExpressionType::Subtract),
            Token::Assign => Ok(BinaryExpressionType::Assign),
            _ => Err(ParserError{
                kind: ParserErrorKind::InvalidBinaryOperatorToken,
                token: token.to_owned()
            })
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralKind {
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String)
}

impl <'a> TryFrom<&'a Token> for LiteralKind {
    type Error = ParserError;

    fn try_from(token: &'a Token) -> Result<Self, Self::Error> {
        match token {
            Token::IntLiteral(i) => Ok(LiteralKind::IntLiteral(i.to_owned())),
            Token::FloatLiteral(f) => Ok(LiteralKind::FloatLiteral(f.to_owned())),
            Token::StringLiteral(s) => Ok(LiteralKind::StringLiteral(s.to_owned())),
            _ =>  Err(ParserError{
                kind: ParserErrorKind::InvalidUnaryOperatorToken,
                token: token.to_owned()
            })
        }
    }
}


/// An enumeration of nodes in the AST
#[derive(Debug, PartialEq, Clone)]
pub enum ASTNode {
    Literal(LiteralKind), // Any type of literal
    Identifier(String), // An identifier
    UnaryExpression { expression_type: UnaryExpressionType, argument: Box<ASTNode> }, // A unary expression
    BinaryExpression { expression_type: BinaryExpressionType, left_argument: Box<ASTNode>, right_argument: Box<ASTNode>}, // A binary expression
    FunctionCall { function: Box<ASTNode>, function_arguments: Vec<Box<ASTNode>>}, // Function call
    ReturnStatement { expression: Box<ASTNode> }, // return statement 
    CodeBlock { lines: Vec<Box<ASTNode>> }, // Multiple Statements
    FunctionDefinition { function_name: Box<ASTNode>, function_arguments: Vec<Box<ASTNode>>, function_code: Box<ASTNode>}
}

/// An Abstract Syntax Tree
#[derive(Debug)]
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

    pub fn new(tokens: &Vec<Token>) -> ASTBuildState {
        ASTBuildState { root_nodes: vec![], tokens: &tokens, current_index: 0 }
    }

}

#[derive(Debug, PartialEq)]
pub enum ParserErrorKind {
    UnexpectedToken,
    EmptyExpression,
    InvalidNumberOfOperatorParameters,
    InvalidUnaryOperatorToken,
    InvalidBinaryOperatorToken,
    InvalidFunctionSignature,
    InvalidLiteral
}

#[derive(Debug, PartialEq)]
pub struct ParserError {
    kind: ParserErrorKind,
    token: Token
}

/// Builds the ast
pub fn build_ast(tokens: &Vec<Token>) -> Result<AST, ParserError> {
    let mut build_state = ASTBuildState::new(tokens);

    while build_state.current_index < tokens.len() {
        match &tokens[build_state.current_index] {
            Token::Space | Token::NewLine => build_state.current_index += 1,
            Token::Define => {
                let function_definition = build_function(&mut build_state)?;
                build_state.root_nodes.push(function_definition);
            }
            ,
             _ => {
                let statement = build_statement(&mut build_state)?;
                build_state.root_nodes.push(statement)
            }
        }
    }

    Ok(AST {root_nodes: build_state.root_nodes})
}

/// Builds an expression
fn build_expression(build_state: &mut ASTBuildState) -> Result<Box<ASTNode>, ParserError> {
    build_expression_recursive(build_state, &get_default_expression_terminal_tokens())
}

/// Builds an expression which may be a function call
fn build_expression_recursive(build_state: &mut ASTBuildState, terminal_tokens: &[Token]) -> Result<Box<ASTNode>, ParserError> {
    let mut expression_fragments: Vec<Vec<Token>> = vec![vec![]];
    let mut was_last_operator = true;

    while let Some(current_token) = build_state.tokens.get(build_state.current_index) {
        if terminal_tokens.contains(current_token) {
            break;
        }
        match current_token {
            Token::LeftParen => { // handle nested parens, just add the tokens to the correct expression fragment
                if !was_last_operator { // If the last token was not an operator, 
                    expression_fragments.push(vec![]);
                }
                was_last_operator = false; // Operator cannot be a result of the parenthesis
                
                add_expressions_in_parenthesis(build_state, expression_fragments.last_mut().unwrap());
                
                continue;
            }
            Token::RightParen => {
                return Err(ParserError{
                    kind: ParserErrorKind::UnexpectedToken,
                    token: current_token.to_owned()
                })
            },
            Token::Space => {
                build_state.current_index += 1;
                continue;
            },
            _ => {}
        }
        if is_operator(&current_token) {
            was_last_operator = true;
            expression_fragments.last_mut().unwrap().push(current_token.to_owned());
        } else if was_last_operator {
            was_last_operator = false;
            expression_fragments.last_mut().unwrap().push(current_token.to_owned());
        } else {
            expression_fragments.push(vec![current_token.to_owned()]);
        }

        build_state.current_index += 1;
    }

    if expression_fragments.len() > 1 {
        Ok(Box::new(ASTNode::FunctionCall {
            function: build_non_function_call_expression(&mut ASTBuildState::new(&expression_fragments[0]), terminal_tokens)?,
            function_arguments: expression_fragments
                .iter()
                .skip(1)
                .map(|tokens| build_non_function_call_expression(&mut ASTBuildState::new(tokens), terminal_tokens))
                .collect::<Result<Vec<Box<ASTNode>>, ParserError>>()?
        }))
    } else {
        build_non_function_call_expression(&mut ASTBuildState::new(&expression_fragments[0]), terminal_tokens)
    }

}

fn add_expressions_in_parenthesis(build_state: &mut ASTBuildState, expression_fragment: &mut Vec<Token>) {
    let mut indentation = 0;
    let parens_terminal_tokens = get_in_parens_expression_terminal_tokens();

    while let Some(current_token) = build_state.tokens.get(build_state.current_index) { // handle indentations
        if parens_terminal_tokens.contains(&current_token) {
            break;
        }
        expression_fragment.push(current_token.to_owned());
        match current_token {
            Token::LeftParen => indentation += 1,
            Token::RightParen => {
                indentation -= 1;
                if indentation == 0 {
                    build_state.current_index += 1;
                    break;
                }
            }
            _ => {}
        }
        build_state.current_index += 1;
    }
}

/// Builds non expressions which are not function calls (can include one in parens)
/// Uses the shunting yard algorithm
fn build_non_function_call_expression(build_state: &mut ASTBuildState, terminal_tokens: &[Token]) -> Result<Box<ASTNode>, ParserError> {
    let mut expr_stack: Vec<Box<ASTNode>> = vec![];
    let mut operator_stack: Vec<Box<Token>> = vec![];
    

    while let Some(current_token) = build_state.tokens.get(build_state.current_index) {
        if terminal_tokens.contains(&current_token) {
            break;
        }
        match current_token {
            Token::FloatLiteral(_) | Token::IntLiteral(_) | Token::StringLiteral(_) => expr_stack.push(Box::new(ASTNode::Literal(current_token.try_into()?))),
            Token::Identifier(ident) => expr_stack.push(Box::new(ASTNode::Identifier(ident.to_owned()))),
            Token::Plus | Token::Minus | Token::ExclamationMark | Token::Assign => {
                shunting_yard_look_for_lower_precedence(current_token, &mut operator_stack, &mut expr_stack)?;
                operator_stack.push(Box::new(current_token.to_owned()));
            }
            Token::LeftParen => {
                build_state.current_index += 1;
                expr_stack.push(build_expression_recursive(build_state, &get_parens_expression_terminal_tokens())?)
            },
            Token::RightParen => {
                return Err(ParserError{
                    kind: ParserErrorKind::UnexpectedToken,
                    token: current_token.to_owned()
                });
            },
            Token::Space => {},
            _ => return Err(ParserError{
                kind: ParserErrorKind::UnexpectedToken,
                token: current_token.to_owned()
            })
        }
        
        build_state.current_index += 1;
    }

    while let Some(stack_top) = operator_stack.pop() {
        push_operation_to_expr_stack(stack_top.as_ref(), &mut expr_stack)?;
    }

    expr_stack.pop().ok_or(ParserError{
                    kind: ParserErrorKind::EmptyExpression,
                    token: Token::Eof
    })
}

fn shunting_yard_look_for_lower_precedence(current_token: &Token, operator_stack: &mut Vec<Box<Token>>, expr_stack: &mut Vec<Box<ASTNode>>) -> Result<(), ParserError>{
    while let Some(stack_top) = operator_stack.pop() {
        if 
            get_operator_precedence(stack_top.as_ref()) < get_operator_precedence(current_token) // If incoming operator has a higher precedence or has the same precedence and is right associative 
            || (                                                                                       // push it on the stack, else pop the stack until it is true and then push on the stack 
                get_operator_precedence(stack_top.as_ref()) == get_operator_precedence(current_token) 
                && get_operator_associativity(current_token) == Associativity::RightToLeft
            ) {
            operator_stack.push(stack_top);
            break;
        }

        push_operation_to_expr_stack(stack_top.as_ref(), expr_stack)?;
    }
    Ok(())
}

fn push_operation_to_expr_stack(operator: &Token, expr_stack: &mut Vec<Box<ASTNode>>) -> Result<(), ParserError> {
    if BinaryExpressionType::try_from(operator).is_ok() {
        let right_expr = expr_stack.pop().ok_or(ParserError{
            kind: ParserErrorKind::InvalidNumberOfOperatorParameters,
            token: operator.to_owned()
        })?;
        let left_expr = expr_stack.pop().ok_or(ParserError{
            kind: ParserErrorKind::InvalidNumberOfOperatorParameters,
            token: operator.to_owned()
        })?;

        expr_stack.push(Box::new(ASTNode::BinaryExpression{ 
            expression_type: operator.try_into()?, 
            left_argument: left_expr, 
            right_argument: right_expr
        }));
    } else if UnaryExpressionType::try_from(operator).is_ok() {
        let expr = expr_stack.pop().ok_or(ParserError{
            kind: ParserErrorKind::InvalidNumberOfOperatorParameters,
            token: operator.to_owned()
        })?;

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
        Token::Assign => 2,
        Token::Plus | Token::Minus => 3,
        Token::ExclamationMark => 5,
        Token::LeftParen | Token::Identifier(_) => 100,
        _ => 0
    }
}

fn get_operator_associativity(token: &Token) -> Associativity {
    if let Ok(binary_expr_type) = BinaryExpressionType::try_from(token) {
        binary_expr_type.get_associativity()
    } else {
        Associativity::LeftToRight
    }
}

/// Returns whether a token represents an operator
fn is_operator(token: &Token) -> bool {
    BinaryExpressionType::try_from(token).is_ok() || UnaryExpressionType::try_from(token).is_ok()
}

fn get_default_expression_terminal_tokens() -> [Token;4] {
    [Token::Eof, Token::NewLine, Token::Unknown, Token::RightCurlyBracket]
}

fn get_parens_expression_terminal_tokens() -> [Token;4] {
    [Token::Eof, Token::RightParen, Token::Unknown, Token::RightCurlyBracket]
}

fn get_in_parens_expression_terminal_tokens() -> [Token;3] {
    [Token::Eof, Token::Unknown, Token::RightCurlyBracket]
}

/// Builds an AST statement 
fn build_statement(build_state: &mut ASTBuildState) -> Result<Box<ASTNode>, ParserError> {
    skip_over_whitespaces(build_state);

    if let Some(current_token) = build_state.tokens.get(build_state.current_index) {
        if let Token::Return = current_token {
            build_state.current_index += 1;
            Ok(Box::new(ASTNode::ReturnStatement { 
                expression: build_expression(build_state)?
            }))
        } else {
            build_expression(build_state)
        }
    } else {
        Err(ParserError {
            kind: ParserErrorKind::EmptyExpression,
            token: Token::Eof
        })
    }
}

/// Builds an AST function
fn build_function(build_state: &mut ASTBuildState) -> Result<Box<ASTNode>, ParserError> {
    verify_starts_with_define(build_state)?;
    build_state.current_index += 1;

    let (function_name, function_arguments) = build_function_signature(build_state)?;
    build_state.current_index += 1;

    let mut statements = vec![];

    let mut indentation = 1;

    while let Some(current_token) = build_state.tokens.get(build_state.current_index) {
        match current_token {
            Token::LeftCurlyBracket => indentation += 1,
            Token::RightCurlyBracket => {
                indentation -= 1;
                if indentation == 0 {
                    build_state.current_index += 1;
                    break;
                }
            }
            _ => statements.push(build_statement(build_state)?)
        }
        build_state.current_index += 1;
        skip_over_whitespaces(build_state);
    }

    Ok(
        Box::new(
            ASTNode::FunctionDefinition {
                function_name: function_name,
                function_arguments: function_arguments,
                function_code: Box::new(
                    ASTNode::CodeBlock {
                        lines: statements
                    }
                )
            }
        )
    )
}

fn verify_starts_with_define(build_state: &mut ASTBuildState) -> Result<(), ParserError> {
    if let Some(Token::Define) = build_state.tokens.get(build_state.current_index) {
        Ok(())
    } else {
        Err(
            ParserError {
                kind: ParserErrorKind::InvalidFunctionSignature,
                token: build_state.tokens.get(build_state.current_index).unwrap_or(&Token::Eof).to_owned()
            }
        )
    }
}

fn build_function_signature(build_state: &mut ASTBuildState) -> Result<(Box<ASTNode>, Vec<Box<ASTNode>>), ParserError> {
    let mut arguments: Vec<Box<ASTNode>> = vec![];

    skip_over_whitespaces(build_state);

    while let Some(current_token) = build_state.tokens.get(build_state.current_index) {
        match current_token {
            Token::Identifier(ident) => arguments.push(Box::new(ASTNode::Identifier(ident.to_owned()))),
            Token::LeftCurlyBracket => break,
            _ => return Err(
                ParserError {
                    kind: ParserErrorKind::UnexpectedToken,
                    token: current_token.to_owned()
                }
            )
        }
        build_state.current_index += 1;
        skip_over_whitespaces(build_state);
    }

    if arguments.len() == 0 {
        Err(
            ParserError {
                kind: ParserErrorKind::InvalidFunctionSignature,
                token: build_state.tokens.get(build_state.current_index).unwrap_or(&Token::Eof).to_owned()
            }
        )
    } else {
        Ok(
            (arguments.first().unwrap().to_owned(), arguments[1..].to_vec())
        )
    }
}

/// Increments the build state's index until it no longer points on a white-space token
fn skip_over_whitespaces(build_state: &mut ASTBuildState) {
    while let Some(Token::NewLine | Token::Space) = build_state.tokens.get(build_state.current_index) {
        build_state.current_index += 1;
    }
}

#[cfg(test)]
mod tests {
    use crate::{tokenizer::Token, parser::{UnaryExpressionType, BinaryExpressionType, LiteralKind}};

    use super::{build_expression, ASTBuildState, ASTNode, ParserError, build_statement, build_function};

    fn get_expression_from_tokens(tokens: &Vec<Token>) -> Result<Box<ASTNode>, ParserError> {
        let mut build_state = ASTBuildState::new(&tokens);
        build_expression(&mut build_state)
    }

    fn get_statement_from_tokens(tokens: &Vec<Token>) -> Result<Box<ASTNode>, ParserError> {
        let mut build_state = ASTBuildState::new(&tokens);
        build_statement(&mut build_state)
    }

    fn get_function_from_tokens(tokens: &Vec<Token>) -> Result<Box<ASTNode>, ParserError> {
        let mut build_state = ASTBuildState::new(&tokens);
        build_function(&mut build_state)
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
                            ASTNode::Identifier("a".to_string())
                        ),
                        right_argument: Box::new(
                            ASTNode::Identifier("b".to_string())
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
        let expression = get_expression_from_tokens(&vec![Token::Identifier("print".to_string()), Token::Space, Token::StringLiteral("Hello World!".to_string())]);

        assert_eq!(
            expression,
            Ok(
                Box::new(
                    ASTNode::FunctionCall {
                        function: Box::new(
                            ASTNode::Identifier("print".to_string())
                        ),
                        function_arguments: vec![
                            Box::new(
                                ASTNode::Literal(
                                    LiteralKind::StringLiteral("Hello World!".to_string())
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
                                LiteralKind::IntLiteral(1)
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
                                    ASTNode::Identifier("a".to_string())
                                ),
                                right_argument: Box::new(
                                    ASTNode::Identifier("b".to_string())
                                )
                            }
                        ),
                        right_argument: Box::new(
                            ASTNode::Identifier("c".to_string())
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
                            ASTNode::Identifier("a".to_string())
                        ),
                        right_argument: Box::new(
                            ASTNode::UnaryExpression {
                                expression_type: UnaryExpressionType::Not,
                                argument: Box::new(
                                    ASTNode::Identifier("b".to_string())
                                )
                            }
                        )
                    }
                )
            ),
            "Failed basic operator precedence: a + !b"
        )
    }

    #[test]
    /// Nested function call: a + (f b)
    fn nested_function_call() {
        let expression = get_expression_from_tokens(&vec![Token::Identifier("a".to_string()), Token::Plus, Token::LeftParen, Token::Identifier("f".to_string()), Token::Space, Token::Identifier("b".to_string()), Token::RightParen]);

        assert_eq!(
            expression,
            Ok(
                Box::new(
                    ASTNode::BinaryExpression {
                        expression_type: BinaryExpressionType::Add,
                        left_argument: Box::new(
                            ASTNode::Identifier("a".to_string())
                        ),
                        right_argument: Box::new(
                            ASTNode::FunctionCall {
                                function: Box::new(
                                    ASTNode::Identifier("f".to_string())
                                ),
                                function_arguments: vec![
                                    Box::new(
                                        ASTNode::Identifier("b".to_string())
                                    )
                                ]
                            }
                        )
                    }

                )
            ),
            "Failed nested function call: a + (f b)"
        )
    }

    #[test]
    /// Expression in function call: f a + b
    fn expression_in_function_call() {
        let expression = get_expression_from_tokens(&vec![Token::Identifier("f".to_string()), Token::Space, Token::Identifier("a".to_string()), Token::Plus, Token::Identifier("b".to_string())]);

        assert_eq!(
            expression,
            Ok(
                Box::new(
                    ASTNode::FunctionCall {
                        function: Box::new(
                            ASTNode::Identifier("f".to_string())
                        ),
                        function_arguments: vec![
                            Box::new(
                                ASTNode::BinaryExpression {
                                    expression_type: BinaryExpressionType::Add,
                                    left_argument: Box::new(
                                        ASTNode::Identifier("a".to_string())
                                    ),
                                    right_argument: Box::new(
                                        ASTNode::Identifier("b".to_string())
                                    )
                                }
                            )
                        ]
                    }
                )
            ),
            "Failed Expression in function call: f a + b"
        )
    }

    #[test]
    /// Nested parenthesis: (a+b+(c-d))+e
    fn nested_parenthesis() {
        let expression = get_expression_from_tokens(&vec![Token::LeftParen, Token::Identifier("a".to_string()), Token::Plus, Token::Identifier("b".to_string()), Token::Plus, Token::LeftParen, Token::Identifier("c".to_string()), Token::Minus, Token::Identifier("d".to_string()), Token::RightParen, Token::RightParen, Token::Plus, Token::Identifier("e".to_string())]);
    
        assert_eq!(
            expression,
            Ok(
                Box::new(
                    ASTNode::BinaryExpression {
                        expression_type: BinaryExpressionType::Add,
                        left_argument: Box::new(
                            ASTNode::BinaryExpression {
                                expression_type: BinaryExpressionType::Add,
                                left_argument: Box::new(
                                    ASTNode::BinaryExpression {
                                        expression_type: BinaryExpressionType::Add,
                                        left_argument: Box::new(
                                            ASTNode::Identifier("a".to_string())
                                        ),
                                        right_argument: Box::new(
                                            ASTNode::Identifier("b".to_string())
                                        )
                                    }
                                ),
                                right_argument: Box::new(
                                    ASTNode::BinaryExpression {
                                        expression_type: BinaryExpressionType::Subtract,
                                        left_argument: Box::new(
                                            ASTNode::Identifier("c".to_string())
                                        ),
                                        right_argument: Box::new(
                                            ASTNode::Identifier("d".to_string())
                                        )
                                    }
                                )
                            }
                        ),
                        right_argument: Box::new(
                            ASTNode::Identifier("e".to_string())
                        )
                    }
                )
            ),
            "Failed nested parenthesis: (a+b+(c-d))+e"
        )
    }

    #[test]
    /// Chain calls: f (g a) b
    fn chain_calls() {
        let expression = get_expression_from_tokens(&vec![Token::Identifier("f".to_string()), Token::Space, Token::LeftParen, Token::Identifier("g".to_string()), Token::Space, Token::Identifier("a".to_string()), Token::RightParen, Token::Space, Token::Identifier("b".to_string())]);

        assert_eq!(
            expression,
            Ok(
                Box::new(
                    ASTNode::FunctionCall {
                        function: Box::new(
                            ASTNode::Identifier("f".to_string())
                        ),
                        function_arguments: vec![
                            Box::new(
                                ASTNode::FunctionCall {
                                    function: Box::new(
                                        ASTNode::Identifier("g".to_string())
                                    ),
                                    function_arguments: vec![
                                        Box::new(
                                            ASTNode::Identifier("a".to_string())
                                        )
                                    ]
                                }
                            ),
                            Box::new(
                                ASTNode::Identifier("b".to_string())
                            )
                        ]
                    }
                )
            ),
            "Failed Chain calls: f (g a) b"
        )
    }


    #[test]
    /// Basic expression statement: a + b
    fn basic_expression_statement() {
        let expression = get_statement_from_tokens(&vec![Token::Identifier("a".to_string()), Token::Plus, Token::Identifier("b".to_string())]); // a + b
        
        assert_eq!(
            expression, 
            Ok(
                Box::new(
                    ASTNode::BinaryExpression {
                        expression_type: BinaryExpressionType::Add,
                        left_argument: Box::new(
                            ASTNode::Identifier("a".to_string())
                        ),
                        right_argument: Box::new(
                            ASTNode::Identifier("b".to_string())
                        )
                    }
                )
            ),
            "Failed Basic expression statement: a + b: a + b"
        );
    }

    #[test]
    /// Basic expression in return statement: return a + b
    fn basic_return_statement() {
        let expression = get_statement_from_tokens(&vec![Token::Return, Token::Space, Token::Identifier("a".to_string()), Token::Plus, Token::Identifier("b".to_string())]); // a + b
        
        assert_eq!(
            expression, 
            Ok(
                Box::new(
                    ASTNode::ReturnStatement {
                        expression: Box::new(
                            ASTNode::BinaryExpression {
                                expression_type: BinaryExpressionType::Add,
                                left_argument: Box::new(
                                    ASTNode::Identifier("a".to_string())
                                ),
                                right_argument: Box::new(
                                    ASTNode::Identifier("b".to_string())
                                )
                            }
                        )
                    }
                )
            ),
            "Failed Basic expression statement: return a + b"
        );
    }

    #[test]
    /// Basic function definition: def f {}
    fn basic_function_definition() {
        let function = get_function_from_tokens(&vec![Token::Define, Token::Space, Token::Identifier("f".to_string()), Token::Space, Token::LeftCurlyBracket, Token::RightCurlyBracket]);

        assert_eq!(
            function,
            Ok(
                Box::new(
                    ASTNode::FunctionDefinition {
                        function_name: Box::new(
                            ASTNode::Identifier("f".to_string())
                        ),
                        function_arguments: vec![],
                        function_code: Box::new(
                            ASTNode::CodeBlock {
                                lines: vec![]
                            }
                        )
                    }
                )
            ),
            "Failed function definition: def f {{}}"
        )
    }

    #[test]
    /// Basic function with arguments: def f a b {}
    fn basic_function_with_arguments() {
        let function = get_function_from_tokens(&vec![Token::Define, Token::Space, Token::Identifier("f".to_string()), Token::Space, Token::Identifier("a".to_string()), Token::Space, Token::Identifier("b".to_string())]);

        assert_eq!(
            function,
            Ok(
                Box::new(
                    ASTNode::FunctionDefinition {
                        function_name: Box::new(
                            ASTNode::Identifier("f".to_string())
                        ),
                        function_arguments: vec![
                            Box::new(
                                ASTNode::Identifier("a".to_string())
                            ),
                            Box::new(
                                ASTNode::Identifier("b".to_string())
                            )
                        ],
                        function_code: Box::new(
                            ASTNode::CodeBlock {
                                lines: vec![]
                            }
                        )
                    }
                )
            ),
            "Basic function with arguments: def f a b {{}}"
        )
    }

    #[test]
    /// Basic function with code: def f {return a + b}
    fn basic_function_with_code() {
        let function = get_function_from_tokens(&vec![Token::Define, Token::Space, Token::Identifier("f".to_string()), Token::Space, Token::LeftCurlyBracket, Token::Return, Token::Space, Token::Identifier("a".to_string()), Token::Plus, Token::Identifier("b".to_string()), Token::RightCurlyBracket]);

        assert_eq!(
            function,
            Ok(
                Box::new(
                    ASTNode::FunctionDefinition {
                        function_name: Box::new(
                            ASTNode::Identifier("f".to_string())
                        ),
                        function_arguments: vec![],
                        function_code: Box::new(
                            ASTNode::CodeBlock {
                                lines: vec![
                                    Box::new(
                                        ASTNode::ReturnStatement {
                                            expression: Box::new(
                                                ASTNode::BinaryExpression {
                                                    expression_type: BinaryExpressionType::Add,
                                                    left_argument: Box::new(
                                                        ASTNode::Identifier("a".to_string())
                                                    ),
                                                    right_argument: Box::new(
                                                        ASTNode::Identifier("b".to_string())
                                                    )
                                                }
                                            )
                                        }
                                    )
                                ]
                            }
                        )
                    }
                )
            ),
            "Failed Basic function with code: def f {{return a + b}}"
        )
    }

    #[test]
    /// Basic function with multiline code: 
    /// def f {
    ///     name = "Oded"
    ///     return name
    /// }
    fn basic_function_with_multiline_code() {
        let function = get_function_from_tokens(&vec![Token::Define, Token::Space, Token::Identifier("f".to_string()), Token::Space, Token::LeftCurlyBracket, Token::Identifier("name".to_string()), Token::Assign, Token::StringLiteral("Oded".to_string()), Token::NewLine, Token::Return, Token::Identifier("name".to_string()), Token::RightCurlyBracket]);

        assert_eq!(
            function,
            Ok(
                Box::new(
                    ASTNode::FunctionDefinition {
                        function_name: Box::new(
                            ASTNode::Identifier("f".to_string())
                        ),
                        function_arguments: vec![],
                        function_code: Box::new(
                            ASTNode::CodeBlock {
                                lines: vec![
                                    Box::new(
                                        ASTNode::BinaryExpression {
                                            expression_type: BinaryExpressionType::Assign,
                                            left_argument: Box::new(
                                                ASTNode::Identifier("name".to_string())
                                            ),
                                            right_argument: Box::new(
                                                ASTNode::Literal(
                                                    LiteralKind::StringLiteral("Oded".to_string())
                                                )
                                            )
                                        }
                                    ),
                                    Box::new(
                                        ASTNode::ReturnStatement {
                                            expression: Box::new(
                                                ASTNode::Identifier("name".to_string())
                                            )
                                        }
                                    )
                                ]
                            }
                        )
                    }
                )
            ),
            r#"Failed Basic function with multiline code: 
             def f {{
                 name = "Oded"
                 return name
             }}"#
        )
    }
}