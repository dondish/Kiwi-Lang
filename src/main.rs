use kiwi_lang::parser::build_ast;
use kiwi_lang::tokenizer::{Token, parse_token};
use nom::error::Error;
use nom::Err;


fn get_all_tokens(input: &str) -> Result<Vec<Token>, Err<Error<&str>>> {
    let mut v = vec![];
    let mut curr_input: &str = input;

    while curr_input.len() > 0 {
        match parse_token(curr_input) {
            Ok((_, Token::Eof)) => {
                v.push(Token::Eof);
                break;
            }
            Ok((rest, token)) => {
                v.push(token); 
                curr_input = rest
            }
            Err(err) => {
                println!("{}", curr_input);
                return Err(err)
            }
        }
    }
    Ok(v)
}

fn main() {
    let code = r#"
    def hello_name name {
        return "Hello " + name
    }
    
    def main {
        integer = 1
        floating_point = 2.53
        string = "Hello World!"
    
        print string
    }"#;
    let tokens = get_all_tokens(code).unwrap();
    let ast = build_ast(&tokens).unwrap();


    println!("------------- Code -------------");
    println!("{}", code);
    println!();
    println!("------------- Tokens -------------");
    println!("{:#?}", tokens);
    println!();
    println!();
    println!("------------- AST -------------");
    println!("{:#?}", ast);
}
