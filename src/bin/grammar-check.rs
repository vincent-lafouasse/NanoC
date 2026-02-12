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

    println!("\n=== START SYMBOL ===");
    if let Some(start_rule) = grammar.rules.first() {
        println!("  {}", start_rule.lhs);
        for (i, prod) in start_rule.productions.iter().enumerate() {
            if i == 0 {
                print!("    -> ");
            } else {
                print!("    |  ");
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

    println!("\n=== RULES ===");
    for rule in grammar.rules.iter().skip(1) {
        println!("  {}", rule.lhs);
        for (i, prod) in rule.productions.iter().enumerate() {
            if i == 0 {
                print!("    -> ");
            } else {
                print!("    |  ");
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

    println!("\n=== VALIDATION ===");
    validate_grammar(&grammar);
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

fn validate_grammar(grammar: &Grammar) {
    use std::collections::HashSet;

    // collect all defined non-terminals (LHS of rules)
    let defined_nonterminals: HashSet<String> =
        grammar.rules.iter().map(|r| r.lhs.clone()).collect();

    // collect all symbols used in productions (RHS)
    let mut used_terminals: HashSet<String> = HashSet::new();
    let mut used_nonterminals: HashSet<String> = HashSet::new();
    let mut undefined_symbols: Vec<String> = Vec::new();

    for rule in &grammar.rules {
        for prod in &rule.productions {
            for symbol in &prod.symbols {
                match symbol {
                    Symbol::Terminal(t) => {
                        used_terminals.insert(t.clone());
                        if !grammar.terminals.contains(t) {
                            undefined_symbols
                                .push(format!("Terminal '{}' used but not declared", t));
                        }
                    }
                    Symbol::NonTerminal(nt) => {
                        used_nonterminals.insert(nt.clone());
                        if !defined_nonterminals.contains(nt) {
                            undefined_symbols
                                .push(format!("Non-terminal '{}' used but not defined", nt));
                        }
                    }
                }
            }
        }
    }

    // check for unused non-terminals (defined but never used in RHS)
    // the start symbol (first rule) is exempt
    let start_symbol = grammar.rules.first().map(|r| r.lhs.as_str());
    let mut unused_nonterminals: Vec<String> = defined_nonterminals
        .iter()
        .filter(|nt| !used_nonterminals.contains(*nt) && Some(nt.as_str()) != start_symbol)
        .cloned()
        .collect();
    unused_nonterminals.sort();

    // check for unused terminals
    let mut unused_terminals: Vec<String> = grammar
        .terminals
        .iter()
        .filter(|t| !used_terminals.contains(*t))
        .cloned()
        .collect();
    unused_terminals.sort();

    // report results
    let mut has_errors = false;

    const RED: &str = "\x1b[91m";
    const YELLOW: &str = "\x1b[93m";
    const GREEN: &str = "\x1b[92m";
    const RESET: &str = "\x1b[0m";

    if !undefined_symbols.is_empty() {
        println!("  {}UNDEFINED SYMBOLS:{}", RED, RESET);
        for msg in &undefined_symbols {
            println!("     {}", msg);
        }
        has_errors = true;
    }

    if !unused_nonterminals.is_empty() {
        println!("  {}UNUSED NON-TERMINALS:{}", YELLOW, RESET);
        for nt in &unused_nonterminals {
            println!("     {}", nt);
        }
    }

    if !unused_terminals.is_empty() {
        println!("  {}UNUSED TERMINALS:{}", YELLOW, RESET);
        for t in &unused_terminals {
            println!("     {}", t);
        }
    }

    if !has_errors && unused_nonterminals.is_empty() && unused_terminals.is_empty() {
        println!(
            "  {}Grammar is valid - all symbols defined and used{}",
            GREEN, RESET
        );
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
            let tokens: Vec<&str> = prod_text.split_whitespace().collect();
            let mut symbols = Vec::new();

            let mut i = 0;
            while i < tokens.len() {
                // skip %prec directives
                if tokens[i] == "%prec" {
                    i += 2; // skip %prec and the following token
                    continue;
                }

                let s = tokens[i];
                if terminals.contains(&s.to_string()) {
                    symbols.push(Symbol::Terminal(s.to_string()));
                } else {
                    symbols.push(Symbol::NonTerminal(s.to_string()));
                }
                i += 1;
            }

            productions.push(Production { symbols });
        }

        rules.push(Rule { lhs, productions });
    }

    rules
}
