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

- **`const` with `undefined` initializer:** `const x: i32 = undefined` is a semantic error.
  `const` always requires a proper expression initializer. Only `var` may use `undefined` or
  `zeroed`.
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
- The condition expression must be an integer type. (Pointers in conditions are not
  implicitly converted — compare explicitly against `zeroed` or a sentinel.)

**`syscall` arguments:**
- The syscall number (first argument) must be an integer type.
- Remaining arguments are unchecked — they map directly to registers and the kernel reads
  what it needs. Passing the wrong number of arguments fills unused registers harmlessly.

#### Lvalue checks

The left-hand side of an assignment, and the operand of `&`, must be an lvalue. Valid lvalues:
- A variable name (both `var` and… see Uncertain for `const`).
- A pointer dereference: `*p`.
- An array index: `a[i]`.
- A field access: `p->field`.

Not lvalues:
- Literals (`42`, `true`).
- Arithmetic expressions (`a + b`).
- Function call results (`foo()`).
- `const` bindings — assigning to a `const` after its declaration is an error.

#### Control flow checks

- **`goto` past a declaration:** a `goto` whose target label appears before a variable
  declaration in the same scope is an error. (Open question §1 — current leaning: static
  error rather than re-execution.)
  ```nanoc
  here:
      var x: i32 = 3;
      goto here;   // ❌ jumps backward past declaration of x
  ```
- **`return` type mismatch:** covered under type checks above.

---

### Uncertain checks

These depend on open design questions or design decisions not yet made. Noted here so they
are not forgotten.

#### Depends on open question §8 — blocks and `if/else` as expressions

If `if/else` and blocks become expressions (current leaning: yes):

- **Branch type agreement:** both branches of an `if/else` expression must produce the same
  type. A mismatch is a type error.
  ```nanoc
  const x: i32 = if (cond) { 42 } else { some_ptr };  // ❌ i32 vs ptr
  ```
- **`if` without `else` in value position:** a bare `if` has type `unit` and cannot appear
  where a value is required.
  ```nanoc
  const x: i32 = if (cond) { 42 };  // ❌ unit where i32 expected
  ```
- **Block type:** a block's type is the type of its last expression (no semicolon). A block
  ending with a statement (semicoloned expression or any other statement) has type `unit`.
- **Unit in value position:** using a `unit`-typed expression where a concrete type is
  expected is a type error.
- **Never type:** `return`, `goto`, and `unreachable` inside an expression context have type
  `never` (bottom type). `never` unifies with any type, allowing:
  ```nanoc
  const x: i32 = if (cond) { 42 } else { return -1; };  // ✅ never unifies with i32
  ```
  This type is internal to the checker and need not be writable by users.

#### Depends on unit-returning function syntax

If `fn foo()` with no `->` means "returns unit" (current leaning: yes, omitted = unit):

- **Exhaustive return checking:** must every code path in a non-unit function end with a
  `return`? This requires control flow analysis (building a CFG and checking that all exit
  paths return). Many compilers implement this as a warning rather than an error, or defer it.
  The check is non-trivial in the presence of `goto`. Deferred until post-MVP.

#### Depends on cast operator decision

NanoC currently has no explicit cast operator. This creates gaps:

- Is `i32 + u8` valid? With no implicit promotion and no cast, you cannot mix types at all,
  which may be too restrictive.
- Is `var x: i32 = some_u32` a type error? Probably yes under strict no-implicit-conversion.
- Without a cast, users have no escape hatch. A cast operator (`x as i32`, or C-style
  `(i32)x`) should be designed before the type checker is finalised. Deferred.

#### Pointer arithmetic rules

`ptr` is currently an opaque, untyped pointer. Typed pointers (`i32*`, `Point*`) also exist.
The rules for mixing them in arithmetic are not yet defined:

- `i32* + i32` → does the compiler scale the offset by `sizeof(i32)`? Or is it byte-level?
- `ptr + i32` → valid? `ptr` has no element size to scale by.
- `i32* - i32*` → valid? Result type?
- Mixing `ptr` and `i32*` in arithmetic or assignment → valid?

These need design decisions before the type checker can enforce them.

#### Variable shadowing

Whether declaring a variable with the same name as one in an outer scope is a hard error,
a warning, or silently allowed is not yet decided. Many systems languages allow it (Rust,
C) with optional warnings; others forbid it.

#### Mutually-referential structs

```nanoc
struct A { b: B*, }
struct B { a: A*, }
```

Pass 1 collects all struct names before resolving field types, so this works as long as the
type checker resolves field types in pass 2 rather than pass 1. No forward declaration syntax
needed, but the implementation must handle the ordering explicitly.
