use std::fs;
use std::path::PathBuf;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: grammar-check <grammar.y>");
        std::process::exit(1);
    }

    let path = PathBuf::from(&args[1]);
    let content = fs::read_to_string(&path).expect("failed to read grammar file");

    let grammar = parse_grammar(&content);

    println!("=== TERMINALS ===");
    for terminal in &grammar.terminals {
        println!("  {}", terminal);
    }

    println!("\n=== PRECEDENCE ===");
    for (level, (assoc, tokens)) in grammar.precedence.iter().enumerate() {
        println!("  Level {}: {:?} -> {:?}", level, assoc, tokens);
    }

    println!("\n=== RULES ===");
    for rule in &grammar.rules {
        println!("  {} ->", rule.lhs);
        for (i, prod) in rule.productions.iter().enumerate() {
            print!("    ");
            if i > 0 {
                print!("| ");
            }
            for symbol in &prod.symbols {
                match symbol {
                    Symbol::Terminal(t) => print!("{} ", t),
                    Symbol::NonTerminal(nt) => print!("{} ", nt),
                }
            }
            println!();
        }
    }
}

#[derive(Debug)]
struct Grammar {
    terminals: Vec<String>,
    precedence: Vec<(Assoc, Vec<String>)>,
    rules: Vec<Rule>,
}

#[derive(Debug)]
enum Assoc {
    Left,
    Right,
    Nonassoc,
}

#[derive(Debug, Clone)]
enum Symbol {
    Terminal(String),
    NonTerminal(String),
}

#[derive(Debug)]
struct Production {
    symbols: Vec<Symbol>,
}

#[derive(Debug)]
struct Rule {
    lhs: String,
    productions: Vec<Production>,
}

fn strip_comments(content: &str) -> String {
    let bytes = content.as_bytes();
    let mut result = String::new();
    let mut i = 0;

    while i < bytes.len() {
        match bytes.get(i) {
            Some(b'/') => match bytes.get(i + 1) {
                Some(b'/') => {
                    // line comment: skip until newline
                    i += 2;
                    while i < bytes.len() && bytes[i] != b'\n' {
                        i += 1;
                    }
                    // keep the newline
                    if i < bytes.len() {
                        result.push('\n');
                        i += 1;
                    }
                }
                Some(b'*') => {
                    // block comment: skip until */
                    i += 2;
                    while i < bytes.len() {
                        if bytes.get(i) == Some(&b'*') && bytes.get(i + 1) == Some(&b'/') {
                            i += 2;
                            break;
                        }
                        i += 1;
                    }
                }
                _ => {
                    result.push(bytes[i] as char);
                    i += 1;
                }
            },
            Some(ch) => {
                result.push(*ch as char);
                i += 1;
            }
            None => break,
        }
    }

    result
}

fn parse_grammar(content: &str) -> Grammar {
    // strip all comments first
    let content = strip_comments(content);

    let mut terminals = Vec::new();
    let mut precedence = Vec::new();

    let mut lines = content.lines();
    let mut in_rules = false;

    for line in lines.by_ref() {
        let trimmed = line.trim();

        // skip empty lines
        if trimmed.is_empty() {
            continue;
        }

        // check for %% separator
        if trimmed == "%%" {
            in_rules = true;
            break;
        }

        // parse %token lines
        if trimmed.starts_with("%token") {
            let tokens = trimmed
                .strip_prefix("%token")
                .unwrap()
                .split_whitespace()
                .map(String::from)
                .collect::<Vec<_>>();
            terminals.extend(tokens);
        }

        // parse precedence lines
        if let Some(rest) = trimmed.strip_prefix("%left") {
            let tokens = rest.split_whitespace().map(String::from).collect();
            precedence.push((Assoc::Left, tokens));
        } else if let Some(rest) = trimmed.strip_prefix("%right") {
            let tokens = rest.split_whitespace().map(String::from).collect();
            precedence.push((Assoc::Right, tokens));
        } else if let Some(rest) = trimmed.strip_prefix("%nonassoc") {
            let tokens = rest.split_whitespace().map(String::from).collect();
            precedence.push((Assoc::Nonassoc, tokens));
        }
    }

    // parse rules after %%
    let rules = if in_rules {
        let rules_text: String = lines.collect::<Vec<_>>().join("\n");
        parse_rules(&rules_text, &terminals)
    } else {
        Vec::new()
    };

    Grammar {
        terminals,
        precedence,
        rules,
    }
}

fn parse_rules(rules_text: &str, terminals: &[String]) -> Vec<Rule> {
    let mut rules = Vec::new();

    // split by semicolons to get individual rules
    for rule_text in rules_text.split(';') {
        let trimmed = rule_text.trim();
        if trimmed.is_empty() {
            continue;
        }

        // split by : to get lhs and rhs
        let parts: Vec<&str> = trimmed.splitn(2, ':').collect();
        if parts.len() != 2 {
            continue; // malformed rule
        }

        let lhs = parts[0].trim().to_string();
        let rhs = parts[1].trim();

        // split by | to get productions
        let mut productions = Vec::new();
        for prod_text in rhs.split('|') {
            let symbols: Vec<Symbol> = prod_text
                .split_whitespace()
                .map(|s| {
                    if terminals.contains(&s.to_string()) {
                        Symbol::Terminal(s.to_string())
                    } else {
                        Symbol::NonTerminal(s.to_string())
                    }
                })
                .collect();

            productions.push(Production { symbols });
        }

        rules.push(Rule { lhs, productions });
    }

    rules
}
