# ADR-0009: Statement-oriented, not expression-oriented

**Status:** Settled
**Area:** Language Design

## Decision

Absolutely settled: this is not an expression-oriented language at all. NanoC is
statement-oriented. Blocks are statement containers with no value. `if/else` and `while`
are statements. Expressions compute values; control flow is handled by statements. This
maps clearly to machine instructions — see the Tour's Philosophy chapter, principle 3
("Fundamentally Procedural and Statement-Oriented").

Bodies of `if`, `else`, and `while` must always be blocks (mandatory braces — see the
Tour's Language Design chapter). `return` is the sole way to produce a value from a
function. No `unit` or `never` types are needed in expression context, since expressions
never need to represent "this diverges" or "this produces nothing" — only statements do,
and statements don't have types at all.

For complex `constexpr` initialization, push the logic into a `comptime fn` (see
[compile-time-conditionals](ADR-0015-compile-time-conditionals.md)) rather than reaching for a
block-as-expression.

This also resolves function/`while` body typing: no implicit return, no unit/never typing
of blocks needed anywhere.

## Historical discussion (preserved)

Two distinct proposals were considered, often conflated. Worth separating, since the
reasoning that ruled each out differs.

### Proposal A — blocks as const/var initializers

Allow a `{ }` block on the right-hand side of a declaration only, producing a value via
its last expression:

```nanoc
const x: i32 = {
    const allocation: MyStruct = make_a_big_object();
    const out: i32 = compute_something_else(&allocation);
    dealloc_thing(&allocation);
    out         // last expression, no semicolon → value of the block
};
```

**What this would have solved:** multi-step initialization with intermediate scratch
values, scoped temporaries that don't leak into the outer block, cleanup before yielding a
value.

**What it would not have solved:** conditional values — `if/else` would still be a
statement, so branching to produce a value would still need a mutable accumulator inside
the block:

```nanoc
const x: i32 = {
    var result: i32 = undefined;
    if (cond) { result = a; } else { result = b; }
    result
};
```

The "last expression without a semicolon is the value" rule would have been contained to
one syntactic position (the right-hand side of a declaration), so it couldn't be triggered
accidentally elsewhere. An alternative considered: explicit `yield` instead of an implicit
last-expression rule, more verbose but removing the semicolon-based footgun entirely.

This also would have sidestepped verifying single-assignment for a `const` conditionally
assigned across branches (which needs real dataflow analysis) — a `const` either has an
initializer expression (including a block) or it doesn't, no post-declaration assignment
is ever valid either way.

### Proposal B — `if/else` as an expression everywhere

Allow `if (cond) { ... } else { ... }` to appear anywhere a value is expected:

```nanoc
const x: i32 = if (cond) { a } else { b };
fn_call(if (flag) { x } else { y });
```

**What this would add over Proposal A:** conditional values without a mutable
accumulator, no need for a ternary `?:` operator, and would make a `comptime if`
(see [compile-time-conditionals](ADR-0015-compile-time-conditionals.md)) work as an expression.

**Additional cost over Proposal A:** `if` would gain two roles (statement and expression),
context-dependent. `return` inside an if-expression branch would unambiguously return from
the *function*, not the `if` — correct, but surprising to read:

```nanoc
const x: i32 = if (cond) { return -1; } else { 42 };
//                         ^^^^^^^^^^
//                         exits the function, not just the if-branch
```

### What "uniform expressions everywhere" would have required

The leaning at one point was toward making all blocks and all `if/else` unconditionally
expressions, not just in initializer position — uniform and simple to specify (every
block produces a value or unit), and `const x: i32 = undefined` would become a parse error
(only `var` could use `undefined`/`zeroed`), with no special initializer-only position to
track in the grammar or sema. This would have needed:

- `if` without `else` in expression position typed `unit`, only valid in statement
  position (value discarded); `if/else` with matching branch types produces that type as
  a value.
- A **never/bottom type** for diverging expressions — `return` and `unreachable` in
  expression context typed `never`, unifying with any type:
  ```nanoc
  const x: i32 = if (cond) { 42 } else { return -1; };
  //                                      ^^^^^^^^^^
  //                                      type: never — unifies with i32, accepted
  ```
  `goto` would stay excluded from expression position even under this design — allowing
  `goto` inside an initializer expression is a footgun since the label target may be in a
  scope where the partially-initialized binding is visible. `return`/`unreachable` are
  safe because they exit the function entirely. The never type wouldn't need to be
  user-facing syntax, just an internal type-checker concept.
- The semicolon rule (`{ stmts; expr }` produces `expr`; `{ stmts; expr; }` produces unit)
  becoming universal rather than confined to one position.

None of this was implemented — the statement-oriented decision above superseded it before
any of this type-system machinery was built.

## History

- Predates version tracking — carried over from the original design document as an
  already-settled decision, with the rejected alternatives kept for context.
