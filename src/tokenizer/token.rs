
use nom::{
    character::{complete::{space1, line_ending, char, digit1, none_of}, complete::anychar}, branch::alt, combinator::{map, value}, IResult, bytes::{complete::{tag, escaped_transform}, complete::take_while1}, number::complete::double, sequence::delimited
};

/**
 * Tokens are the first step of processing the script.
 * 
 * They represent the lowest level of the scripting language
 */
#[derive(Debug)]
pub enum Token {
    Space,
    NewLine,
    LeftCurlyBracket, // {
    RightCurlyBracket, // }
    Plus, // +
    Minus, // -
    FunctionDeclaration, // def
    Assign, // =
    IntLiteral(i64), // 1
    FloatLiteral(f64), // 3.14
    Identifier(String),
    StringLiteral(String), // "Hello",

    Unknown // Anything else that doesn't match ($ for example)
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
       write!(f, "{:?}", self)
    }
}

macro_rules! token {
    ($token:expr) => {
        |_| $token 
    };
}

pub fn parse_token(input: &str) -> IResult<&str, Token> {
    alt((
        map(space1, token!(Token::Space)),
        map(line_ending, token!(Token::NewLine)),
        map(char('{'), token!(Token::LeftCurlyBracket)),
        map(char('}'), token!(Token::RightCurlyBracket)),
        map(char('+'), token!(Token::Plus)),
        map(char('-'), token!(Token::Minus)),
        map(tag("def"), token!(Token::FunctionDeclaration)),
        map(char('='), token!(Token::Assign)),
        map(double, |d| Token::FloatLiteral(d)),
        map(digit1, resolve_int_literal),
        map(take_while1(is_identifier_character), |s: &str| Token::Identifier(s.to_string())),
        map(
            delimited(
                char('"'), 
                escaped_transform(
                        take_while1(|c: char| c != '\\' && c != '"'), 
                        '\\', 
                        alt((
                            value("\\", tag("\\")),
                            value("\"", tag("\"")),
                            value("\n", tag("n")),
                            value("\t", tag("t"))
                        ))
                    ),
                char('"')
            ),
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