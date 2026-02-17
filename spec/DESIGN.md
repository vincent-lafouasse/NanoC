# NanoC Design Document

**Version:** 0.1.0
**Last Updated:** 2026-02-17

This document describes the design philosophy, architecture, and implementation details of NanoC for contributors

---

## Table of Contents

1. [Philosophy](#philosophy)
2. [Language Design](#language-design)
3. [Compiler Architecture](#compiler-architecture)
4. [Implementation Details](#implementation-details)
5. [Testing Strategy](#testing-strategy)
6. [Contributing](#contributing)
7. [Open Questions](#open-questions)

---

## Philosophy

### Core Principles

**1. Minimal**
NanoC is intentionally small. Every language feature must justify its existence. We prefer explicit over implicit, and simple over convenient.

**2. Context-Free Grammar**
The grammar must be context-free and unambiguous. This makes parsing straightforward and reasoning about code easier.

**3. Readable Assembly**
There should be a predictable, almost 1:1 mapping from NanoC to RISC-V assembly. Programmers should be able to mentally compile their code.

**4. No Runtime**
Direct syscalls, no libc dependency. The compiler generates pure assembly with no runtime overhead.

**5. Simple Calling Convention**
Register-sized arguments only. No hidden copies, no complex ABI rules.

**6. Explicit Everything**
No implicit conversions, no type coercion, no hidden allocations. What you see is what you get.

---

## Language Design

### Type System

#### Primitive Types
- `u8` - unsigned 8-bit integer
- `i32` - signed 32-bit integer
- `u32` - unsigned 32-bit integer
- `ptr` - opaque pointer (raw address)

**Design Decision:** No floating point. NanoC targets systems programming where explicit integer operations are preferred.

#### Pointers
Syntax: `i32*`, `Point**`, `u8***`

#### Structs
```c
struct Point {
    x: i32,
    y: i32,
}
```

Structs define aggregate types. Fields must end with commas (even the last one) for grammar simplicity.

**Critical Rule: Structs are NEVER passed by value.**

- Keeps calling convention simple
- Makes memory semantics visible
- Avoids hidden copies
- Eliminates ABI complexity

```c
fn process(p: Point*) { ... }  // ✅ Correct
fn process(p: Point) { ... }   // ❌ Compile error
```

### Variables and Initialization

#### Mandatory Initialization
Every variable MUST have an initializer:

```c
var x: i32 = 10;           // Expression initializer
var buffer: u8* = zeroed;  // Zero initialization
var temp: i32 = undefined; // Uninitialized (danger!)
```

**Design Decision:** No uninitialized-by-default variables. Forces programmers to think about initialization.

The `undefined` keyword makes undefined behavior explicit rather than accidental.

**Rule:** `const` variables cannot use `undefined` (checked at compile time).

### Control Flow

#### Structured Control
- `if`/`else` - conditional execution
- `while` - loops
- `goto`/labels - arbitrary jumps

**No `break` or `continue` keywords.**

**Rationale:** These are redundant with `goto`. In nested loops, `break` is ambiguous about which loop to exit. `goto` is explicit:

```c
while (x > 0) {
    while (y > 0) {
        if (done) goto cleanup;  // Clear intent
    }
}
cleanup:
```

#### Goto Restrictions

**Open Design Question:** Should we allow backward jumps past variable declarations?

```c
here:
    var x: i32 = 3;  // What happens on second iteration?
    goto here;
```

C allows this (re-executes initialization). We should either:
1. Allow it (match C behavior)
2. Static error (prevent confusion)
3. Require declarations before labels in each scope

**Current leaning:** Static error for backward jumps past declarations.

### Functions

#### Signature Restrictions
```c
fn add(a: i32, b: i32) -> i32 { ... }
```

**Arguments:** Must be `register_sized` (primitives or pointers)
**Return values:** Currently allows any `type`, but should be restricted to `register_sized` for consistency

**Why register-sized only?**
- Simple calling convention
- Predictable register usage (a0-a7 for args, a0-a1 for return)
- No stack-spilling complexity
- Makes ABI trivial

#### Multi-Value Returns

**Open Design Question:** How to use both return registers (a0, a1)?

**Option 1: Tuple returns** (recommended)
```c
fn divmod(a: i32, b: i32) -> (i32, i32) {
    return (a / b, a % b);
}

var quot: i32;
var rem: i32;
(quot, rem) = divmod(10, 3);
```

**Option 2: Output parameters** (explicit but doesn't use both registers)
```c
fn divmod(a: i32, b: i32, quot: i32*, rem: i32*) {
    *quot = a / b;
    *rem = a % b;
}
```

**Current status:** Not yet decided. Tuple syntax is cleaner and maps directly to registers.

### Operators

All standard C operators with standard precedence:
- Arithmetic: `+`, `-`, `*`, `/`, `%`
- Bitwise: `&`, `|`, `^`, `~`, `<<`, `>>`
- Comparison: `==`, `!=`, `<`, `<=`, `>`, `>=`
- Logical: `&&`, `||`, `!`
- Pointer: `&` (address-of), `*` (deref), `->` (field access)

**No operator overloading.** Each operator has exactly one meaning.

### Defined Behavior (C Undefined Behavior)

NanoC makes several behaviors well-defined that C leaves undefined or implementation-defined. This makes code more predictable and easier to reason about.

**Quick Reference:**

| Behavior            | C  | NanoC                          |
|---------------------|----|--------------------------------|
| Signed overflow     | UB | Two's complement wrap          |
| Division by zero    | UB | Returns -1                     |
| Signed right shift  | ID | Arithmetic shift               |
| Left shift negative | UB | Bitwise operation              |
| Evaluation order    | US | Left-to-right                  |
| Uninitialized vars  | UB | UB (explicit with `undefined`) |
| Array out-of-bounds | UB | UB (same as C)                 |
| Null dereference    | UB | UB (same as C)                 |

UB: Undefined Behavior
ID: Implementation-Defined
US: Unspecified

#### Signed Integer Overflow
**C behavior:** Undefined behavior
**NanoC behavior:** Two's complement wrapping (defined)

```c
const MAX: i32 = 2147483647;
var x: i32 = MAX + 1;  // x = -2147483648 (wraps, defined)
```

**Rationale:** RISC-V integers are two's complement. Undefined behavior here serves no optimization purpose and makes overflow bugs harder to reason about.

#### Division by Zero
**C behavior:** Undefined behavior
**NanoC behavior:** Returns -1 (defined)

```c
var x: i32 = 10 / 0;  // x = -1 (defined, not UB)
var y: i32 = 10 % 0;  // y = -1 (defined, not UB)
```

**Rationale:** Hardware division instructions on RISC-V don't trap on divide-by-zero - they return specific values. We expose this behavior rather than adding checks. `-1` is the RISC-V spec behavior for division by zero.

#### Signed Right Shift
**C behavior:** Implementation-defined (arithmetic or logical shift)
**NanoC behavior:** Arithmetic shift (sign-extends)

```c
var x: i32 = -8 >> 1;  // x = -4 (arithmetic shift, defined)
```

**Rationale:** RISC-V has distinct arithmetic (`sra`) and logical (`srl`) shift instructions. We always use arithmetic shift for signed types, logical for unsigned.

#### Left Shift of Negative Numbers
**C behavior:** Undefined behavior
**NanoC behavior:** Bitwise operation (defined)

```c
var x: i32 = -1 << 2;  // x = -4 (defined)
```

**Rationale:** Shifts are bitwise operations. The bit pattern shifts left regardless of sign interpretation. Two's complement makes this well-defined.

#### Evaluation Order
**C behavior:** Unspecified for most operators
**NanoC behavior:** Left-to-right (defined)

```c
fn side_effect() -> i32 { ... }
var x: i32 = side_effect() + side_effect();  // left call happens first
```

**Rationale:** Predictable evaluation order makes debugging easier and removes subtle bugs. We always evaluate left operand before right operand.

#### Out-of-Bounds Array Access
**C behavior:** Undefined behavior
**NanoC behavior:** Still undefined behavior (for now)

**Open question:** Should we define this? Options:
1. Keep as UB (performance)
2. Trap/panic (safety)
3. Return zero (defined but dangerous)

**Current decision:** UB (same as C). May add optional bounds checking later.

#### Null Pointer Dereference
**C behavior:** Undefined behavior
**NanoC behavior:** Undefined behavior

**Rationale:** Null dereferences cause page faults in hardware. We don't add overhead to check every pointer access. Use explicit checks if needed.

#### Uninitialized Memory
**C behavior:** Undefined behavior (implicit)
**NanoC behavior:** Explicit with `undefined` keyword

```c
var x: i32 = undefined;  // Explicitly undefined (not implicit!)
```

**Rationale:** While the behavior is still undefined, making it explicit prevents *accidental* undefined behavior. You must write `undefined` to opt into UB.

---

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

**File:** `src/lexer.rs`

The lexer is complete and well-tested:
- Scans source bytes into tokens
- Tracks spans (position, line, column)
- Binary-search keyword lookup
- Comprehensive error handling
- 40+ unit tests

**Invariant:** The `KEYWORDS` array must be sorted (checked at runtime in constructor).

**Implementation note:** The lexer uses `Rc<[u8]>` for source sharing without copying.

### Parser Details

**File:** `src/parser.rs`

**Current status:**
- ✅ Type parsing (primitives, pointers, structs)
- ✅ Struct declarations
- ✅ Variable declarations with `zeroed`/`undefined`
- 🚧 Expression parsing (Pratt algorithm - TODO)
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

**Next steps:**
1. Implement `parse_expression_bp()` (binding power recursion)
2. Add prefix parsing (atoms, unary ops)
3. Add infix parsing (binary ops)
4. Add postfix parsing (field access, array index, calls)

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

## Implementation Details

### Memory Management

**Shared ownership:** `Rc<[u8]>` for source code and identifiers
- Zero-copy string sharing
- Reference counting for cleanup
- Immutable after creation

**AST allocation:** `Box<T>` for recursive structures
- Prevents infinite size types
- Heap allocation for tree nodes

### Performance Considerations

**Current priority:** Correctness over speed

The compiler should:
- Parse 10,000 lines in < 100ms
- Compile simple programs in < 1s
- Keep memory under 100MB for typical files

**Not priorities:**
- Parallel compilation
- Incremental compilation
- Build caching

### Error Message Quality

**Good error format:**
```
source.nc:15:8: error: type mismatch
  expected: i32
  found:    u32*
15 |     var x: i32 = ptr;
   |              ^
```

**Requirements:**
- Show file, line, column
- Display relevant source line
- Point to exact error location
- Explain what was expected vs found
- Suggest fixes where possible

---

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

## Open Questions

### Language Design

1. **Backward goto past declarations:** Allow or forbid?
   - C allows (re-initializes)
   - Could be confusing
   - **Proposal:** Static error

2. **Tuple returns and error handling:** Multiple proposals for using both RISC-V return registers (a0, a1)

   **Background:** RISC-V calling convention provides two return registers (a0 and a1). Most functions only use a0. We could use both registers for richer return semantics.

   **Proposal A: Simple tuples (minimal)**
   ```c
   fn divmod(a: i32, b: i32) -> (i32, i32) {
       return (a / b, a % b);
   }

   var quot: i32;
   var rem: i32;
   (quot, rem) = divmod(10, 3);
   ```
   - Pro: Minimal syntax, explicit
   - Con: Doesn't address error handling ergonomics

   **Proposal B: Tuple-based Result pattern (convention)**
   ```c
   // Convention: (value, error_code)
   // a0 = value (or garbage if error)
   // a1 = error code (0 = success)

   fn divide(a: i32, b: i32) -> (i32, i32) {
       if (b == 0) {
           return (0, 1);  // ERR_DIV_ZERO
       }
       return (a / b, 0);  // value, no error
   }

   var result: i32;
   var err: i32;
   (result, err) = divide(10, 0);
   if (err != 0) {
       // Handle error
   }
   ```
   - Pro: Uses both registers, explicit error handling
   - Con: Very verbose for error propagation

   **Proposal C: Zig-style `try` (ergonomic)**
   ```c
   fn process_file() -> (i32, i32) {
       const fd = try open_file("data.txt");  // Auto-propagate on error
       const data = try read_data(fd);
       const result = try process(data);
       return (result, 0);
   }

   // `try` desugars to:
   // var fd: i32;
   // var err: i32;
   // (fd, err) = open_file("data.txt");
   // if (err != 0) { return (undefined, err); }
   ```
   - Pro: Ergonomic, explicit keyword, makes error handling practical
   - Con: Adds keyword, implicit control flow (early return)
   - Similar to Zig's error handling (proven in practice)

   **Current leaning:**
   - Proposal A (simple tuples) for general use
   - Either skip Result pattern entirely (too verbose without `try`)
   - OR add `try` keyword (Proposal C) if error handling becomes important

   **Key insight:** Without `try` semantics, the Result pattern is too verbose to be practical. Either commit to `try` or stick with traditional C error codes.

3. **Array syntax:** Static arrays in grammar but not implemented
   - Should be `i32[10]` or `[10]i32`?
   - Stack allocated only?
   - **Proposal:** `i32[10]` (postfix, matches pointers)

4. **String literals:** Currently `ptr` type
   - Should be `u8*` for safety?
   - Null-terminated?
   - **Proposal:** Unsure if we want null termination

5. **Array bounds checking:** Leave as UB or define behavior?
   - UB (performance, matches C)
   - Trap/panic (safety)
   - Return zero (defined but misleading)
   - **Current:** UB (same as C), may add optional checking

### Implementation

1. **Error recovery:** Should parser attempt to continue after errors?
   - Pro: Report multiple errors at once
   - Con: More complex parser
   - **Current:** Fail fast (report first error only)

2. **Optimization passes:** Add any?
   - Dead code elimination?
   - Constant folding?
   - **Current:** None (keep codegen simple)

3. **Debug info:** Generate DWARF?
   - Useful for debugging
   - Adds complexity
   - **Proposal:** Later, not MVP

---

## Appendix

### Grammar Summary

See `spec/grammar.ebnf` for full grammar.

Key productions:
```ebnf
program = top_level_statement* ;

top_level_statement = struct_decl
                    | var_decl
                    | function_decl
                    ;

register_sized = primitive_type | pointer ;

var_decl = ("var" | "const") variable_name ":" type "=" var_initializer ";" ;
var_initializer = expression | "zeroed" | "undefined" ;

statement = var_decl
          | expression ";"
          | "return" expression? ";"
          | "if" "(" expression ")" statement ("else" statement)?
          | "while" "(" expression ")" statement
          | "{" statement* "}"
          | "goto" label ";"
          | label ":" statement
          ;
```

### References

- RISC-V Spec: https://riscv.org/technical/specifications/
- Pratt Parsing: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
- C89 Standard (for comparison)

---

**Document Status:** Living document, updated as design evolves.

**Next Review:** After Pratt parser implementation.
