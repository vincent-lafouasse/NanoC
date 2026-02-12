#![allow(dead_code)]

use nanoc::lexer::{Lexer, Token};

use std::process::exit;

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

    let source_bytes = source.as_bytes();
    dbg!(&source);

    let mut lexer = Lexer::new(source_bytes);
    let mut token_count = 0;

    loop {
        match lexer.next_token() {
            Ok((token, new_lexer)) => {
                println!("{:?}", token);

                token_count += 1;
                lexer = new_lexer;

                if token == Token::Eof {
                    break;
                }
            }
            Err(err) => {
                eprintln!("Lexer error: {:?}", err);
                exit(1);
            }
        }
    }

    println!("\n{} tokens lexed successfully", token_count);
}
