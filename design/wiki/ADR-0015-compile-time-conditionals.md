# ADR-0015: Compile-time conditionals

**Status:** Open
**Area:** Future Work
**Leaning:** depends entirely on [ADR-0008](ADR-0008-binding-forms-var-constexpr.md), which is no longer settled itself

**Not currently implemented.** Needed for portable code that targets multiple platforms
(RISC-V vs ARM, bare-metal vs Linux, debug vs release).

## The need

Without compile-time branching, a program that must behave differently per target is
forced to maintain separate source files or hardcode one platform. Canonical example —
platform-specific intrinsics:

```nanoc
// want this on riscv64:
fn flush_icache() { fence_i(); }

// want this on aarch64:
fn flush_icache() { isb(); }
```

## Design

Three keywords, proposed but not settled — see
[binding-forms-var-constexpr](ADR-0008-binding-forms-var-constexpr.md), which this whole
design leans on:

```nanoc
var   x: i32 = expr;      // mutable, stored
const x: i32 = expr;      // (removed — see binding-forms-var-constexpr)
constexpr x: i32 = expr;  // immutable, folded, compile-time only — not addressable
```

**`comptime fn`** — marks a function as evaluable at compile time. Called normally at
runtime; evaluated by the compiler when it appears in a `constexpr` initializer or
`comptime if` condition:

```nanoc
comptime fn page_count(size: i32, page: i32) -> i32 {
    return (size + page - 1) / page;
}

constexpr PAGES: i32 = page_count(65536, 4096);  // evaluated at compile time → 16
var buf: ptr = malloc(page_count(n, 4096));      // called normally at runtime
```

`comptime` and `inline` (see [inline-keyword](ADR-0007-inline-keyword.md)) are orthogonal:
`comptime inline fn` is evaluated at compile time in constexpr/comptime-if contexts *and*
always inlined at runtime call sites.

**`comptime fn` sema restrictions:**
- May only call other `comptime` functions — no runtime-only calls, no `syscall`.
- Parameters must be `constexpr`-typed when called from a comptime context.
- No `undefined`/`zeroed` local initializers — every value must be statically known.
- No `while` loops initially — termination can't be proved; relax later if needed.
- No global mutable state access.

**`constexpr` initializer rule:** must be a *constexpr expression* — a literal, a
`constexpr` binding, or a call to a `comptime fn` with `constexpr` arguments. Sema error
otherwise. `&constexpr_binding` is always a sema error (no storage exists to point at).

**Compiler-provided target constants** — always `constexpr`, defined by the compiler:

```
ARCH    "riscv64" | "aarch64"
OS      "linux" | "bare"
OPT     "debug" | "release"
```

**`comptime if`** — the `comptime` keyword preceding an `if_expr`. The condition must be a
constexpr expression. Dead branches are still parsed and type-checked (unlike the C
preprocessor). May appear at statement level or in expression position:

```nanoc
// statement level — selects between function definitions
comptime if (ARCH == "riscv64") {
    fn flush_icache() { fence_i(); }
} else {
    fn flush_icache() { isb(); }
}

// expression level — selects a compile-time value
constexpr PAGE_SIZE: i32 = comptime if (ARCH == "riscv64") { 4096 } else { 65536 };
```

`comptime` does not apply to arbitrary expressions (unlike Zig) — compile-time evaluation
only occurs in `constexpr` initializers and `comptime if` conditions.

## Open sub-questions

- Do symbols declared inside a `comptime if` block (statement level) escape into the
  enclosing scope?
- Can `comptime if` appear inside a function body, or only at top level?
- Can `comptime fn` contain `if/else` expressions? (Probably yes — they're pure.)
- How does this interact with [module-system-and-ffi](ADR-0013-module-system-and-ffi.md)?

## History

- Predates version tracking — carried over from the original design document. Originally
  recorded as settled design; corrected — depends entirely on ADR-0008, which turned out
  not to be settled either, so this can't be more settled than its foundation is.
