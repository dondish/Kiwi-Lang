
use nom::{
    character::{streaming::{space1, line_ending, char, digit1}, streaming::{anychar, one_of}}, branch::alt, combinator::{map, eof, recognize, opt, map_res, value}, IResult, bytes::{streaming::tag, streaming::take_while1}, number::streaming::double, sequence::{tuple, terminated, preceded}, multi::{many1, many0}
};

use std::str::FromStr;
use super::string_parser::parse_string;

/**
 * Tokens are the first step of processing the script.
 * 
 * They represent the lowest level of the scripting language
 */
#[derive(Debug, Clone, PartialEq)]
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

/// Parses a single token from the input.
pub fn parse_token(input: &str) -> IResult<&str, Token> {
    alt((
        value(Token::Eof, eof),
        value(Token::Space, space1),
        value(Token::NewLine, line_ending),
        value(Token::LeftCurlyBracket, char('{')),
        value(Token::LeftParen, char('(')),
        value(Token::RightCurlyBracket, char('}')),
        value(Token::RightParen, char(')')),
        value(Token::Plus, char('+')),
        value(Token::Minus, char('-')),
        value(Token::ExclamationMark, char('!')),
        value(Token::Define, tag("def")),
        value(Token::Return, tag("return")),
        value(Token::Assign, char('=')),
        map(parse_double, Token::FloatLiteral),
        map(parse_integer, Token::IntLiteral),
        map(take_while1(is_identifier_character), |s: &str| Token::Identifier(s.to_string())),
        map(
            parse_string,
            |s| Token::StringLiteral(s)
        ),
        value(Token::Unknown, anychar)
    ))(input)
}

/// Floating number parser, Uses code from https://github.com/Geal/nom/blob/main/doc/nom_recipes.md#floating-point-numbers
fn parse_double(input: &str) -> IResult<&str, f64> {
    map_res(alt((
        // Case one: .42
        recognize(
          tuple((
            char('.'),
            decimal,
            opt(tuple((
              one_of("eE"),
              opt(one_of("+-")),
              decimal
            )))
          ))
        )
        , // Case two: 42e42 and 42.42e42
        recognize(
          tuple((
            decimal,
            opt(preceded(
              char('.'),
              decimal,
            )),
            one_of("eE"),
            opt(one_of("+-")),
            decimal
          ))
        )
        , // Case three: 42. and 42.42
        recognize(
          tuple((
            decimal,
            char('.'),
            opt(decimal)
          ))
        )
      )),
    |s| f64::from_str(s))(input)
}

/// Integer parser, uses code from https://github.com/Geal/nom/blob/main/doc/nom_recipes.md#integers
fn parse_integer(input: &str) -> IResult<&str, i64> {
    alt((
        map_res(
            hexadecimal, 
            |s| i64::from_str_radix(s, 16)
        ),
        map_res(
            octal, 
            |s| i64::from_str_radix(s, 8)
        ),
        map_res(
            binary, 
            |s| i64::from_str_radix(s, 2)
        ),
        map_res(
            decimal,
            |s| i64::from_str(s)
        )
    ))(input)
}

/// Decimal integer parser
fn decimal(input: &str) -> IResult<&str, &str> {
    recognize(
        many1(
        terminated(one_of("0123456789"), many0(char('_')))
        )
    )(input)
}

/// Hexadecimal integer parser
fn hexadecimal(input: &str) -> IResult<&str, &str> { // <'a, E: ParseError<&'a str>>
  preceded(
    alt((tag("0x"), tag("0X"))),
    recognize(
      many1(
        terminated(one_of("0123456789abcdefABCDEF"), many0(char('_')))
      )
    )
  )(input)
}

/// Octal integer parser
fn octal(input: &str) -> IResult<&str, &str> {
    preceded(
      alt((tag("0o"), tag("0O"))),
      recognize(
        many1(
          terminated(one_of("01234567"), many0(char('_')))
        )
      )
    )(input)
}

/// Binary integer parser
fn binary(input: &str) -> IResult<&str, &str> {
    preceded(
      alt((tag("0b"), tag("0B"))),
      recognize(
        many1(
          terminated(one_of("01"), many0(char('_')))
        )
      )
    )(input)
  }

fn is_identifier_character(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}