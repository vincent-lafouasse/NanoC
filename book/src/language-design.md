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
```nanoc
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

```nanoc
fn process(p: Point*) { ... }  // ✅ Correct
fn process(p: Point) { ... }   // ❌ Compile error
```

### Variables and Initialization

#### One binding keyword: `var`

There is no `const` keyword for runtime bindings. `var` is the only way to declare a
variable. Immutability is a convention enforced by the programmer, not the compiler — the
same way C was before `const` existed. There is no compile-time enforcement of read-only
access to runtime variables.

Compile-time constants use `constexpr` and are a separate concept (see Future Work).

#### Mandatory Initialization
Every variable MUST have an initializer:

```nanoc
var x: i32 = 10;           // expression initializer
var buffer: u8* = zeroed;  // zero-initialised
var temp: i32 = undefined; // explicitly uninitialised (danger!)
```

**Design Decision:** No uninitialized-by-default variables. Forces explicit acknowledgement
of uninitialized state. The `undefined` keyword makes the hazard visible rather than silent.

#### Declarations at the top of scope

All `var` (and `constexpr`) declarations in a block must precede all statements. Interleaving
declarations and statements is a parse error:

```nanoc
fn example() {
    var x: i32 = 10;    // ✅ declaration section
    var y: i32 = 20;    // ✅
    x = x + y;          // ✅ statement section
    var z: i32 = 0;     // ❌ parse error — declaration after statement
}
```

**Rationale:** This rule eliminates the `goto`-past-declaration hazard entirely. If all
declarations are at the top of a scope and all labels are in the statement section, no
`goto` within that scope can ever skip an initialisation. The `goto` analysis simplifies
from "did this jump cross a declaration?" to "does this jump target a label inside a nested
block?" — a much cheaper check.

### Control Flow

#### Structured Control
- `if`/`else` - conditional execution
- `while` - loops
- `goto`/labels - arbitrary jumps

**No `break` or `continue` keywords.**

#### Condition types

An `if` or `while` condition must be a register-sized expression: `u8`, `i32`, `u32`, or
any pointer. Structs are not valid conditions.

The condition compiles to a register load followed by a branch-if-zero or
branch-if-not-zero instruction (`BEQ`/`BNE` against `x0`). No separate boolean type or
implicit boolean conversion exists — the hardware already does it. Smaller types (`u8`)
are zero-extended by the load; pointers are register-sized natively.

```nanoc
if (x)    { ... }  // ✅ i32 — non-zero is true
if (ptr)  { ... }  // ✅ pointer — non-null is true
if (node) { ... }  // ✅ pointer — non-null check, zero cost
```

#### Mandatory braces

The then-clause of `if` and the body of `while` must always be a block. The else-clause
may be either a block or another `if` statement — the minimal exception needed for
`else if` chains:

```nanoc
if (cond) { x = 1; }         // ✅ then-clause is a block
if (cond) x = 1;             // ❌ parse error — then-clause must be a block
while (cond) { ... }         // ✅
while (cond) stmt;           // ❌ parse error
if (a) { } else { }         // ✅ else-clause is a block
if (a) { } else if (b) { }  // ✅ else-clause is another if — chaining
if (a) { } else x = 1;      // ❌ parse error — else-clause must be block or if
```

**Rationale:** This eliminates the dangling `else` ambiguity entirely. After `else` the
parser sees either `{` (block) or `if` (chained if) — nothing else is valid, so there is
nothing to be ambiguous about. `else if` chains require no special syntax:

```nanoc
if (x > 0) {
    positive
} else if (x < 0) {
    negative
} else {
    zero
}
```

**Rationale:** These are redundant with `goto`. In nested loops, `break` is ambiguous about which loop to exit. `goto` is explicit:

```nanoc
while (x > 0) {
    while (y > 0) {
        if (done) goto cleanup;  // Clear intent
    }
}
cleanup:
```

#### Expression Statements

Only function calls and syscalls are allowed as expression statements. Arbitrary expressions like `f() + g();` or `x * 2;` are rejected at parse time — the outermost expression of an expression statement must be a `nanocall` or `Syscall`.

```nanoc
my_func(a, b);                        // ✅ direct call
my_interface->vtable[5](args);        // ✅ complex call expression, but outermost is still a call
syscall(93, 0);                       // ✅ syscall
f() + g();                            // ❌ parse error: not a call
x * 2;                                // ❌ parse error: not a call
```

**Rationale:** An expression statement that isn't a call is almost always a bug — the result is discarded. If two functions have side effects, call them separately: `f(); g();`. This catches mistakes like writing `a == b;` when you meant `a = b;` without needing a separate lint pass.

#### Goto Restrictions

**Open Design Question:** Should we allow backward jumps past variable declarations?

```nanoc
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
```nanoc
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

**Option 1: Tuple returns** (simple, minimal)
```nanoc
fn divmod(a: i32, b: i32) -> (i32, i32) {
    return (a / b, a % b);
}

var quot: i32;
var rem: i32;
(quot, rem) = divmod(10, 3);
```

**Option 2: Some kind of zig style error ** (could be cool but useless without some kind of `try` mechanics)
```nanoc
fn can_fail() -> (x: i32, Error) {
    try can_also_fail(x);

    if (x == 0) {
        return Err(my_error(x));
    }

    return Ok(x);
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

**No operator overloading.** Each operator has exactly one meaning.

#### Array Indexing

`lhs[rhs]` always means: index the array or pointer `lhs` by `rhs`. The left operand must be the pointer or array type.

**Design Decision:** `int[pointer]` is not allowed.

In C, `a[i]` is defined as `*(a + i)`, so `i[a]` is technically valid since addition is commutative. NanoC does not inherit this — `[]` is an indexing operation, not pointer arithmetic sugar.

```nanoc
arr[i]   // ✅ pointer/array on the left
i[arr]   // ❌ compile error: left operand of [] must be a pointer or array
```

**Rationale:** The symmetry in C is an accidental consequence of the `*(a+i)` expansion, not a deliberate feature. It serves no practical purpose and makes type-checking more complex (both sides must be considered as potential pointer). Requiring the pointer on the left makes intent unambiguous and the type rule simple: the left operand is indexed, the right operand is the index.

### Defined Behavior (C Undefined Behavior)

NanoC makes several behaviors well-defined that C leaves undefined or implementation-defined. This makes code more predictable and easier to reason about.

**Quick Reference:**

| Behavior            | C  | NanoC                          |
|---------------------|----|--------------------------------|
| Signed overflow     | UB | Two's complement wrap          |
| Division by zero    | UB | UB (same as C)                 |
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

```nanoc
const MAX: i32 = 2147483647;
var x: i32 = MAX + 1;  // x = -2147483648 (wraps, defined)
```

**Rationale:** RISC-V integers are two's complement. Undefined behavior here serves no optimization purpose and makes overflow bugs harder to reason about.

#### Division by Zero
**C behavior:** Undefined behavior
**NanoC behavior:** Undefined behavior (same as C)

```nanoc
var x: i32 = 10 / 0;  // UB
var y: i32 = 10 % 0;  // UB
```

**Rationale:** Integer division by zero is trivially detectable before the operation. Defining a return value would mask bugs rather than expose them. Users should guard explicitly:

```nanoc
if (b != 0) {
    var result: i32 = a / b;
}
```

#### Signed Right Shift
**C behavior:** Implementation-defined (arithmetic or logical shift)
**NanoC behavior:** Arithmetic shift (sign-extends)

```nanoc
var x: i32 = -8 >> 1;  // x = -4 (arithmetic shift, defined)
```

**Rationale:** RISC-V has distinct arithmetic (`sra`) and logical (`srl`) shift instructions. We always use arithmetic shift for signed types, logical for unsigned.

#### Left Shift of Negative Numbers
**C behavior:** Undefined behavior
**NanoC behavior:** Bitwise operation (defined)

```nanoc
var x: i32 = -1 << 2;  // x = -4 (defined)
```

**Rationale:** Shifts are bitwise operations. The bit pattern shifts left regardless of sign interpretation. Two's complement makes this well-defined.

#### Evaluation Order
**C behavior:** Unspecified for most operators
**NanoC behavior:** Left-to-right (defined)

```nanoc
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

```nanoc
var x: i32 = undefined;  // Explicitly undefined (not implicit!)
```

**Rationale:** While the behavior is still undefined, making it explicit prevents *accidental* undefined behavior. You must write `undefined` to opt into UB.

---

