## Semantic Analysis

Semantic analysis runs after parsing and before code generation. It is the last phase that can
produce user-facing errors. If it passes, code generation is a deterministic, infallible
translation.

### Passes

Two passes over the AST (see Compiler Architecture for rationale):

**Pass 1 — declaration collection.** Walk top-level items only. Do not recurse into function
bodies. Collect into the symbol table:
- All struct definitions (name → field list with types)
- All function signatures (name → parameter types + return type)
- All global variable declarations (name → type)

**Pass 2 — resolution and checking.** Walk the full AST. Resolve every identifier to a symbol
table entry. Perform all checks listed below.

---

### Certain checks

These are required regardless of any open design questions.

#### Name resolution

- **Undefined name:** any reference to a variable, function, or type that was not declared.
- **Duplicate declaration:** two declarations of the same name in the same scope —
  applies to variables, functions, structs, struct fields, and goto labels within a function.
- **Variable used before declaration:** within a function body, a name may not be referenced
  before its declaration statement. (Labels are exempt — they are collected in a pre-pass over
  the function body so that forward `goto` works.)
- **`goto` to undefined label:** a `goto` whose target label does not exist anywhere in the
  enclosing function.

#### Declaration checks
- **Struct passed by value:** a function parameter or return type that is a bare struct type
  (not a pointer) is an error. All struct access must go through pointers.
  ```nanoc
  fn process(p: Point*)  { ... }  // ✅
  fn process(p: Point)   { ... }  // ❌ struct by value
  fn get() -> Point      { ... }  // ❌ struct by value
  ```
- **Non-register-sized function parameter:** parameter types must be primitives (`u8`, `i32`,
  `u32`) or pointers. Struct types are excluded by the rule above.
- **Recursive `inline` function:** a function marked `inline` that calls itself directly or
  transitively is an error. Inlining a recursive call would not terminate.
- **Address-of an `inline` function:** taking `&fn_name` where `fn_name` is `inline` is an
  error. An inlined function has no stable address.

#### Type checks

**Assignment:**
- The RHS expression type must exactly match the declared type of the LHS. No implicit
  conversions.

**Binary operators:**
- Arithmetic (`+`, `-`, `*`, `/`, `%`) and bitwise (`&`, `|`, `^`, `<<`, `>>`): both operands
  must be integer types and must match each other.
- Comparison (`==`, `!=`, `<`, `<=`, `>`, `>=`): both operands must be the same type.
- Logical (`&&`, `||`): both operands must be integer types (NanoC has no boolean type;
  zero is false, nonzero is true).

**Unary operators:**
- `!`, `~`: operand must be an integer type.
- Unary `-`: operand must be an integer type.
- `&` (address-of): operand must be an lvalue (see below).
- `*` (dereference): operand must be a pointer type. Result type is the pointed-to type.

**Field and index access:**
- `->`: left operand must be a pointer-to-struct. Right operand must be a valid field name of
  that struct.
- `[]`: left operand must be a pointer type; right operand must be an integer type. The
  left operand being an integer and right a pointer is rejected (no `i[arr]` — see Language
  Design).

**Function calls:**
- Argument count must match the declared parameter count.
- Each argument type must match the corresponding parameter type exactly.
- The call expression's type is the function's return type.

**`return` statement:**
- The type of the returned expression must match the function's declared return type.
- `return` with no expression is valid only in a unit-returning function (no `->` annotation).
- `return expr` in a unit-returning function is an error.

**`if`/`while` condition:**
- The condition must have a register-sized type: `u8`, `i32`, `u32`, or any pointer.
  Structs are not valid conditions — they don't fit in a register.
- The value is loaded into a register and the branch tests it against zero (`BEQ`/`BNE`
  with `x0`). This is not a type conversion — it is what the load instruction does.
  `u8` is zero-extended; `i32`/`u32` fill the register directly; pointers are
  register-sized natively.
- `if (ptr)` is a valid null check at zero cost — no explicit `!= zeroed` required.

**`syscall` arguments:**
- The syscall number (first argument) must be an integer type.
- Remaining arguments are unchecked — they map directly to registers and the kernel reads
  what it needs. Passing the wrong number of arguments fills unused registers harmlessly.

#### Lvalue checks

The left-hand side of an assignment, and the operand of `&`, must be an lvalue. Valid lvalues:
- A `var` name — the only runtime binding (see `design/wiki/ADR-0008-binding-forms-var-constexpr.md`).
- A pointer dereference: `*p`.
- An array index: `a[i]`.
- A field access: `p->field`.

Not lvalues:
- Literals (`42`, `true`).
- Arithmetic expressions (`a + b`).
- Function call results (`foo()`).
- `constexpr` bindings — not stored, so there's no storage to assign to or take the
  address of; both are sema errors.

#### Control flow checks

- **`goto` into a nested block:** a `goto` whose target label is inside a nested block
  (inner `{ }`) is an error. Since all declarations precede all statements within every
  block, a `goto` within the same scope or to an outer scope can never skip an
  initialisation. Only jumping *into* a nested scope is hazardous (it bypasses that
  scope's declaration section).
  ```nanoc
  goto inner;         // ❌ jumps into nested block, skipping its declarations
  if (cond) {
      var x: i32 = 0;
  inner:
      return x;
  }
  ```
- **`return` type mismatch:** covered under type checks above.

---

### Statement orientation

NanoC is statement-oriented. Blocks are statement containers with no value. `if/else` and
`while` are statements. Expressions compute values; they do not contain control flow.

No `unit` or `never` types are needed in expression context. `return`, `goto`, and
`unreachable` are statements only — they cannot appear inside an expression. (See
`design/wiki/ADR-0009-statement-oriented-expressions.md` for the alternatives this ruled out.)

### Mutually-referential structs

```nanoc
struct A { b: B*, }
struct B { a: A*, }
```

Pass 1 collects all struct names before resolving field types, so this works as long as the
type checker resolves field types in pass 2 rather than pass 1. No forward declaration syntax
needed, but the implementation must handle the ordering explicitly.

### Gaps depending on open design questions

A handful of checks can't be specified yet because they depend on decisions not yet made —
noted here so they're not forgotten, full discussion in the wiki:

- **Exhaustive return checking** — must every code path in a non-unit function end with a
  `return`? Depends on control-flow analysis in the presence of `goto`.
  See `design/wiki/ADR-0018-exhaustive-return-checking.md`.
- **Cast operator** — with no implicit conversions and no cast operator, users have no
  escape hatch for mixing types. See `design/wiki/ADR-0017-type-system-gaps.md`.
- **Pointer arithmetic rules** — how typed pointers (`i32*`) and the opaque `ptr` type mix
  in arithmetic isn't defined yet. See `design/wiki/ADR-0017-type-system-gaps.md`.
- **Variable shadowing** — whether shadowing an outer-scope binding is an error, a warning,
  or allowed isn't decided. See `design/wiki/ADR-0017-type-system-gaps.md`.
