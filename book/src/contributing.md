## Contributing

### Code Style

**Rust conventions:**
- Use `rustfmt` (default settings)
- No `unsafe` unless absolutely necessary
- Prefer `Result` over `Option` for errors
- Document public APIs with `///` comments

**Naming:**
- Types: `PascalCase`
- Functions: `snake_case`
- Constants: `SCREAMING_SNAKE_CASE`
- Modules: `snake_case`

### Adding Language Features

**Process:**
1. Discuss design in issue
2. Update grammar in `spec/grammar.ebnf`
3. Add tokens to lexer (if needed)
4. Update parser
5. Add tests (parser tests + integration test)
6. Update this document

**Example:** Adding a new keyword
1. Add to `KEYWORDS` array in `lexer.rs` (keep sorted!)
2. Add `TokenType` variant
3. Update parser to handle new syntax
4. Add tests

---

