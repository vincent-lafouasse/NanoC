# ADR-0012: Compiler warnings

**Status:** Open (proposed)
**Area:** Implementation

## Rationale

NanoC aims for simplicity, but some code patterns are error-prone or surprising even in a
minimal language. Warnings can help catch mistakes without adding language complexity —
they're purely optional and don't affect compilation, and could be enabled/disabled via
flags (`-Wall`, `-Wno-precedence`) implemented as a separate analysis pass after parsing.

## Proposed warnings

### a) Precedence warnings — HIGH priority

Warn where bitwise operators mix with comparison/equality, since most programmers expect
bitwise AND to bind tighter than equality, C's own precedence (inherited from B) puts
equality above bitwise operators, and even experienced programmers get this wrong:

```nanoc
// Warning: '&' has lower precedence than '=='; '==' will be evaluated first
if (x & 0xFF == 0) { ... }
//    ^~~~~~~~~~~
// Suggestion: Use parentheses: (x & 0xFF) == 0
```

Similar warnings for `x | FLAG == y`, `x ^ y == 0`, `a < b & mask`.

### b) Unused variable warnings

```nanoc
var temp: i32 = compute();  // Warning: variable 'temp' is unused
```

### c) `constexpr` binding never read

```nanoc
constexpr MAX: i32 = 100;  // Warning: constant 'MAX' is never used
```

(Originally written against `const`, before [const was removed](ADR-0008-binding-forms-var-constexpr.md);
applies to `constexpr` now.)

### d) Suspicious assignment in condition

```nanoc
if (x = 5) { ... }  // Warning: using assignment in condition (did you mean '=='?)
```

Moot as written today since NanoC doesn't support assignment-as-expression at all (see
[assignment-as-expression](ADR-0003-assignment-as-expression.md), current leaning: statement only) —
this warning would only become relevant if that decision ever flipped.

### e) Unreachable code

```nanoc
fn example() -> i32 {
    return 42;
    var x: i32 = 0;  // Warning: unreachable code
}
```

### f) Function declared but never called

For non-exported functions:

```nanoc
fn helper() -> i32 {  // Warning: function 'helper' is never used
    return 5;
}
```

### g) Division by literal zero — HIGH priority, candidate for hard error

Division by zero is UB in NanoC (see the Tour's Language Design chapter). When the divisor
is a literal `0`, the compiler can catch it statically:

```nanoc
var x: i32 = a / 0;   // Error: division by zero
var y: i32 = a % 0;   // Error: division by zero
```

**Proposal:** promote this to a hard compile error rather than a warning — there's no
legitimate use case for dividing by a literal zero, and a warning would wrongly suggest
the code might be acceptable. Only applies to literal `0`; division by a variable whose
runtime value happens to be zero remains UB, the programmer's responsibility to guard
against.

## Status

Precedence warnings: HIGH priority, genuinely confusing. Division-by-literal-zero:
HIGH priority, candidate for a hard error rather than a warning. The rest: medium
priority, nice to have, not blocking anything.

## History

- Predates version tracking — carried over from the original design document, none
  implemented yet.
