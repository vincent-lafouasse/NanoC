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

#### Expression Statements

Only function calls and syscalls are allowed as expression statements. Arbitrary expressions like `f() + g();` or `x * 2;` are rejected at parse time — the outermost expression of an expression statement must be a `Call` or `Syscall`.

```c
my_func(a, b);                        // ✅ direct call
my_interface->vtable[5](args);        // ✅ complex call expression, but outermost is still a call
syscall(93, 0);                       // ✅ syscall
f() + g();                            // ❌ parse error: not a call
x * 2;                                // ❌ parse error: not a call
```

**Rationale:** An expression statement that isn't a call is almost always a bug — the result is discarded. If two functions have side effects, call them separately: `f(); g();`. This catches mistakes like writing `a == b;` when you meant `a = b;` without needing a separate lint pass.

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

**Option 1: Tuple returns** (simple, minimal)
```c
fn divmod(a: i32, b: i32) -> (i32, i32) {
    return (a / b, a % b);
}

var quot: i32;
var rem: i32;
(quot, rem) = divmod(10, 3);
```

**Option 2: Some kind of zig style error ** (could be cool but useless without some kind of `try` mechanics)
```c
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

```c
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

```c
const MAX: i32 = 2147483647;
var x: i32 = MAX + 1;  // x = -2147483648 (wraps, defined)
```

**Rationale:** RISC-V integers are two's complement. Undefined behavior here serves no optimization purpose and makes overflow bugs harder to reason about.

#### Division by Zero
**C behavior:** Undefined behavior
**NanoC behavior:** Undefined behavior (same as C)

```c
var x: i32 = 10 / 0;  // UB
var y: i32 = 10 % 0;  // UB
```

**Rationale:** Integer division by zero is trivially detectable before the operation. Defining a return value would mask bugs rather than expose them. Users should guard explicitly:

```c
if (b != 0) {
    var result: i32 = a / b;
}
```

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

