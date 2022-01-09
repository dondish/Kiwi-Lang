
use nom::{
    character::{streaming::{space1, line_ending, char, digit1}, streaming::anychar}, branch::alt, combinator::{map, eof}, IResult, bytes::{streaming::tag, streaming::take_while1}, number::streaming::double
};

use super::string_parser::parse_string;

/**
 * Tokens are the first step of processing the script.
 * 
 * They represent the lowest level of the scripting language
 */
#[derive(Debug, Clone)]
pub enum Token {
    Space,
    NewLine,
    LeftCurlyBracket, // {
    LeftParen, // (
    RightCurlyBracket, // }
    RightParen, // )
    Plus, // +
    Minus, // -
    ExclamationMark, // !
    Define, // def
    Return, // return
    Assign, // =
    IntLiteral(i64), // 1
    FloatLiteral(f64), // 3.14
    Identifier(String),
    StringLiteral(String), // "Hello",

    Eof, // End of file
    Unknown // Anything else that doesn't match ($ for example)
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
       write!(f, "{:?}", self)
    }
}

/// Macro for returning no argument tokens from map
macro_rules! token {
    ($token:expr) => {
        |_| $token 
    };
}

/// Parses a single token from the input.
pub fn parse_token(input: &str) -> IResult<&str, Token> {
    alt((
        map(eof, token!(Token::Eof)),
        map(space1, token!(Token::Space)),
        map(line_ending, token!(Token::NewLine)),
        map(char('{'), token!(Token::LeftCurlyBracket)),
        map(char('('), token!(Token::LeftParen)),
        map(char('}'), token!(Token::RightCurlyBracket)),
        map(char(')'), token!(Token::RightParen)),
        map(char('+'), token!(Token::Plus)),
        map(char('-'), token!(Token::Minus)),
        map(char('!'), token!(Token::ExclamationMark)),
        map(tag("def"), token!(Token::Define)),
        map(tag("return"), token!(Token::Return)),
        map(char('='), token!(Token::Assign)),
        map(double, |d| Token::FloatLiteral(d)),
        map(digit1, resolve_int_literal),
        map(take_while1(is_identifier_character), |s: &str| Token::Identifier(s.to_string())),
        map(
            parse_string,
            |s| Token::StringLiteral(s)
        ),
        map(anychar, token!(Token::Unknown))
    ))(input)
}

fn resolve_int_literal(s: &str) -> Token {
    Token::IntLiteral(s.parse::<i64>().unwrap())
}

fn is_identifier_character(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}