## Compiler Architecture

### Pipeline Overview

```
Source (.nc file)
    ↓
┌───────────────┐
│ Lexer         │  ← Tokens with spans (fully implemented)
└───────────────┘
    ↓
┌───────────────┐
│ Parser        │  ← AST with spans (partially implemented)
└───────────────┘     - Structs: ✅ done
    ↓                 - Variables: ✅ done
┌───────────────┐     - Expressions: 🚧 Pratt parsing TODO
│ Semantic      │     - Functions: ❌ not started
│ Analysis      │  ← Type checking, name resolution (not started)
└───────────────┘
    ↓
┌───────────────┐
│ Code          │  ← RISC-V assembly (not started)
│ Generation    │
└───────────────┘
    ↓
Assembly (.s file)
```

### Error Handling Philosophy

**Errors by stage:**

1. **Lexer:** Fallible - reports character-level errors with spans
2. **Parser:** Fallible - reports syntax errors with spans
3. **Semantic Analysis:** Fallible - reports type/scope/flow errors with spans
4. **Codegen:** Infallible - panics on bugs (ICEs)

Codegen should NEVER produce user-facing errors.

If semantic analysis passes, codegen is deterministic translation. Any codegen failure is a compiler bug, not a user error.

**Source Metadata:** Spans (line/column info) are kept through semantic analysis for error reporting, then dropped before/during codegen.

### Lexer Details

**File:** `lib/lexer.ml`

**Status (v0.2.0): MVP complete.** Scans source into tokens with line/column spans,
covering every keyword, operator, and punctuation token currently in the language, plus
all four literal kinds (string, char, int/unsigned/byte/ptr) with their full escape
tables and lexer-side range checking (see `design/wiki/ADR-0005-string-literals-and-escape-sequences.md`
and `design/wiki/ADR-0019-integer-literals.md`). Covered by `test/test_lexer.ml` (unit
tests, one per token kind plus error cases), `test/test_atoi.ml` (the int64
bounds-checking helper), and `test/test_lexer_integration.ml` (whole realistic program
snippets tokenized end to end).

A handful of gaps are known and deliberately deferred rather than blocking: `\dDDD`
decimal escapes raise instead of returning a proper error, unescaped newlines inside
string/char literals aren't yet the hard error ADR-0022 settles, and the `constexpr`/
`unreachable`/`true`/`false` keyword tokens don't exist yet. None of these block moving
on to the parser.

### Parser Details

**File:** `src/parser.rs`

**Architecture:** Hybrid approach
- **Top-level & statements:** Ad-hoc recursive descent parser
- **Expressions:** Pratt parser (operator precedence climbing)

**Rationale:** NanoC's dirt-simple context-free grammar makes recursive descent trivial for most constructs. Only expressions need the sophistication of Pratt parsing to handle precedence and associativity elegantly. No need for a full parser generator when the grammar is this straightforward.

**Current status:**
- ✅ Type parsing (primitives, pointers, structs)
- ✅ Struct declarations
- ✅ Variable declarations with `zeroed`/`undefined`
- 🚧 Expression parsing (Pratt algorithm - partial implementation)
- ❌ Function declarations
- ❌ Statements (if/while/blocks)

**Type Wrappers:**
```rust
pub struct TypeName { rc: Rc<[u8]> }
pub struct VariableName { rc: Rc<[u8]> }
```

These provide type safety (can't confuse a type name with a variable name) and convenience methods (`as_bytes()`, `as_str()`).

### Expression Parsing (Pratt Algorithm)

**Status:** Prepared but not implemented

**Precedence levels:**
```rust
None = 0       // Entry point
Assignment = 1 // =
LogicalOr = 2  // ||
LogicalAnd = 3 // &&
BitwiseOr = 4  // |
BitwiseXor = 5 // ^
BitwiseAnd = 6 // &
Equality = 7   // == !=
Comparison = 8 // < > <= >=
Shift = 9      // << >>
Term = 10      // + -
Factor = 11    // * / %
Unary = 12     // ! ~ - &
Postfix = 13   // -> . [] ()
```

**Associativity:** Implicit in Pratt parsing
- Left-associative: recurse with `precedence + 1`
- Right-associative (assignment): recurse with `precedence`

**Testing:** S-expression output format for parse trees
```
3 + 4 * 5  →  (+ 3 (* 4 5))
-x + y     →  (+ (- x) y)
a && b || c → (|| (&& a b) c)
```

### Semantic Analysis (Not Started)

**Responsibilities:**
- Type checking (type compatibility, no implicit conversions)
- Name resolution (undefined variables, duplicate names)
- Scope tracking (variable shadowing, goto targets)
- Control flow analysis (unreachable code, backward jumps past decls)
- Function signature validation (register-sized params)
- Const validation (no `undefined` initializer)

**Output:** Typed AST ready for codegen

**Error reporting:** Use spans from AST nodes for detailed diagnostics

### Code Generation (Not Started)

**Target:** RISC-V RV32I or RV64I

**Calling Convention:**
- Arguments: a0-a7 (first 8 register-sized values)
- Return: a0 (or a0+a1 if tuples are added)
- Callee-saved: s0-s11
- Caller-saved: t0-t6

**Stack Layout:**
```
High addresses
    ┌─────────────┐
    │ Return addr │  ← ra saved here
    ├─────────────┤
    │ Saved regs  │  ← s0-s11 if used
    ├─────────────┤
    │ Local vars  │  ← sp points here
    └─────────────┘
Low addresses
```

**Struct handling:**
- Structs never passed by value
- All struct access via pointers
- Field access: base + offset calculation

**Codegen strategy:**
- Single pass (no optimization initially)
- Naive register allocation (spill often)
- Predictable output (readability over efficiency)

---

