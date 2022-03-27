use std::collections::HashMap;

use kiwi_lang::parser::build_ast;
use kiwi_lang::runner::{Runner, Value, RunError, ExternalFunction};
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

fn print(args: &Vec<Value>) -> Result<Value, RunError> {
    println!("{}", args.iter().map(|v| v.to_string()).collect::<Vec<_>>().join(" "));
    Ok(Value::Int(0))
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
    
        print string integer + floating_point
        print (hello_name "Oded")
    }
    "#;
    // let code = r#"
    //     x = "Circumference of circle with radius 5: "

    //     pi = 3.141592653589
    //     radius = 5

    //     print x 3 + 5
    // "#.trim();
    let tokens = get_all_tokens(code).unwrap();
    let ast = build_ast(&tokens).unwrap();
    let external_functions = HashMap::from([("print".to_owned(), print as ExternalFunction)]);
    let mut runner = Runner::new(&external_functions);

    println!("------------- Code -------------");
    println!("{}", code);
    println!();
    println!("------------- Tokens -------------");
    println!("{:#?}", tokens);
    println!();
    println!();
    println!("------------- AST -------------");
    println!("{:#?}", &ast);
    println!();
    println!("------------- Run -------------");
    match runner.run(&ast) {
        Err(e) => println!("{:#?}", e.kind),
        _ => {}
    }
}
