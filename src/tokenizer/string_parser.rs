/// Inspired by https://github.com/Geal/nom/blob/main/examples/string.rs

use nom::{sequence::{delimited, preceded}, character::streaming::{char, multispace1}, IResult, multi::fold_many0, branch::alt, combinator::{map, verify, value, map_res, map_opt}, bytes::streaming::{is_not, tag, take_while_m_n}};

/// Parses a string literal from the input
pub fn parse_string(input: &str) -> IResult<&str, String> {
    let build_string = fold_many0(
        parse_fragment, 
        String::new, 
        |mut string, fragment| {
            match fragment {
                StringFragment::Literal(s) => string.push_str(s),
                StringFragment::EscapedChar(c) => string.push(c),
                StringFragment::EscapedWS => {}
            }
            string
        }
    );

    delimited(char('"'), build_string, char('"'))(input)
}

/// The string is split to fragments that are concatenated together
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StringFragment<'a> {
    Literal(&'a str), // Any non-escaped regular string
    EscapedChar(char), // \u{04} \t \b \n and all of the escaped characters
    EscapedWS // Allows escaping of whitespace in strings to create multiline strings
}

/// Parses a string fragment from the input
fn parse_fragment<'a>(input: &'a str) -> IResult<&str, StringFragment<'a>> {
    alt((
        map(parse_literal, StringFragment::Literal),
        map(parse_escaped_char, StringFragment::EscapedChar)
    ))(input)
}

// Parses string literals without escapes
fn parse_literal(input: &str) -> IResult<&str, &str> {
    let not_quote_slash = is_not("\\\"");

    verify(not_quote_slash, |s : &str| s.is_empty())(input)
}

// Parses escaped characters
fn parse_escaped_char(input: &str) -> IResult<&str, char> {
    preceded(
        char('\\'),
        alt((
            parse_unicode,
            value('\\', char('\\')),
            value('\r', char('r')),
            value('"', char('"')),
            value('\n', char('n')),
            value('\t', char('t')),
            value('\u{08}', char('b')),
            value('\u{0C}', char('f')),
            value('\\', char('\\')),
            value('/', char('/')),
        )) 
    )(input)
}

/// Parses uXXXXXX to characters
fn parse_unicode(input: &str) -> IResult<&str, char> {
    let parse_hex = take_while_m_n(1, 6, |c: char| c.is_ascii_hexdigit()); // parse hex digits
    
    let parse_preceded_hex = preceded( // Parse literals like uXXXXXX
        char('u'),
        parse_hex
    );

    let parse_u32 = map_res(parse_preceded_hex, |hex| u32::from_str_radix(hex, 16)); // convert hex to u32

    map_opt(parse_u32, |value| std::char::from_u32(value))(input) // convert u32 to char
}

// Parses escaped whitespace
fn parse_escaped_whitespace(input: &str) -> IResult<&str, &str> {
    preceded(char('\\'), multispace1)(input)
}