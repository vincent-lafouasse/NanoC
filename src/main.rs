#![allow(dead_code)]

use nanoc::lexer::{Lexer, TokenType};
use std::process::exit;
use std::rc::Rc;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <file.nc>", args[0]);
        eprintln!("   or: {} -c <code>", args[0]);
        exit(1);
    }

    // Get source code from either file or -c flag
    let source = if args[1] == "-c" {
        if args.len() < 3 {
            eprintln!("Error: -c requires a code string");
            exit(1);
        }
        args[2].clone()
    } else {
        let filename = &args[1];
        match std::fs::read_to_string(filename) {
            Ok(content) => content,
            Err(err) => {
                eprintln!("Error reading file '{}': {}", filename, err);
                exit(1);
            }
        }
    };

    dbg!(&source);

    let source_rc: Rc<[u8]> = source.as_bytes().into();
    let mut lexer = Lexer::new(source_rc);
    let mut token_count = 0;

    loop {
        match lexer.next_token() {
            Ok(token) => {
                println!("{:?}", token);

                token_count += 1;

                if matches!(token.kind, TokenType::Eof) {
                    break;
                }
            }
            Err(err) => {
                eprintln!("{}", err.format(&source));
                exit(1);
            }
        }
    }

    println!("\n{} tokens lexed successfully", token_count);
}
