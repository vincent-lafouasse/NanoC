## Testing Strategy

### Unit Tests

**Lexer:** Test individual token types, edge cases, errors
- 40+ tests covering all token types
- Error formatting validation
- Edge cases (hex, binary, escapes)

**Parser:** Test one top-level statement per test
- Struct declarations (simple, nested, errors)
- Variable declarations (types, initializers)
- Expression trees (S-expression validation)

**Pattern:** `parse_one()` helper ensures exactly one statement parsed

### Integration Tests

**End-to-end:** Source → Assembly
- Example programs in `examples/`
- Compile and run on RISC-V simulator
- Verify output matches expected

### S-Expression Testing

For expression parsing, use Lisp-style output:
```rust
assert_eq!(parse_expr("3 + 4 * 5"), "(+ 3 (* 4 5))");
```

Makes precedence and associativity bugs obvious.

---

