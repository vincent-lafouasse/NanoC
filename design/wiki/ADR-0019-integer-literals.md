# ADR-0019: Integer literals

**Status:** Open
**Area:** Language Design
**Leaning:** syntax settled (i32 default, u suffix); range-check hard-error recommended, not confirmed — leaning toward a u32 magnitude bounds check at lexing, full i32-aware bounds check at parsing

## Syntax

Bare digits default to `i32`; a `u` suffix marks the literal `u32`:

```nanoc
42            // i32
42u           // u32
0
0u
2147483647    // i32 max
4294967295u   // u32 max
```

Decimal digits only for now — no `0x`/`0b` prefixes.

## Open — how does a `u8`-typed literal get written?

There's no stated way to target `u8` specifically. Under strict no-implicit-conversion
(and with no cast operator — see [cast-operator](ADR-0021-cast-operator.md)), a bare
literal is `i32` by default, which doesn't type-check against a `u8` context at all.
Either `u8` needs its own literal form, or this waits on whatever the cast operator ends
up looking like.

## Open — literal range checking

Should a literal that doesn't fit its type (`4294967296u`, one past `u32` max) be a
static error?

This is a different question from runtime arithmetic overflow, which is already settled
(wraps, no check — see the Tour's Defined Behavior table): a per-operation check costs
something on every add/sub/mul, and runtime wraparound is sometimes genuinely intentional
(checksums, hashing). Neither reason applies to a literal — checking it costs nothing
(once, at compile time, not per-operation), and there's no legitimate use case for a
literal that silently means something other than what it says; nobody writes
`4294967296u` intending "wrap to 0."

**Recommendation, not yet confirmed:** a literal out of range for its type is a hard
compile error, exactly the same reasoning already applied to division by a literal zero
(see [compiler-warnings](ADR-0012-compiler-warnings.md) — candidate for a hard error,
while the runtime case, dividing by a variable that happens to be zero, stays UB). Runtime
arithmetic overflow is untouched by this either way.

### Gotcha to handle when this gets implemented: `i32::MIN`

The grammar never lexes a negative numeric token — `-` is `prefix_op` (see
[grammar.ebnf](../grammar.ebnf)), never part of `INTEGER_LITERAL`. So `-2147483648` always
lexes as `Minus, IntLiteral 2147483648`, and `2147483648` is the *magnitude*, which by
itself is one past `i32::MAX` (`2147483647`) and would wrongly fail a naive per-literal
range check — even though `-2147483648` is exactly `i32::MIN`, perfectly in range. C has
the identical wart, which is why `INT_MIN` is defined as `-2147483647 - 1` rather than a
bare literal in `<limits.h>`.

Whatever implements the range check will need to special-case the AST shape
`Negate(IntLiteral n)` and check `-n` against the range, rather than checking the bare
literal `n` in isolation — a parser/sema-level pattern match, not something the lexer needs
to know about.

### Where the check actually lives: split across lexer and parser

Tempting shortcut considered and rejected: have the lexer itself recognize `-` immediately
followed by digits as a single signed literal token, sidestepping the `i32::MIN` gotcha by
just never separating sign from magnitude in the first place. Doesn't work — the ambiguity
isn't in what follows the `-`, it's in what precedes it. `-5` (wants a signed literal) and
`a - 5` (wants `Identifier a; Minus; IntLiteral 5`) both have `-` immediately followed by a
digit; only the *previous* token distinguishes them (does it end an expression, making `-`
binary, or not, making it unary). A single-pass, forward-only lexer has no previous-token
memory to check that without effectively growing parser-shaped state — and even then it'd
just be duplicating what the parser's prefix/infix (nud/led) dispatch already decides, with
strictly less context available. The grammar's existing choice stands: `-` always lexes as
its own `Minus` token (see [grammar.ebnf](../grammar.ebnf)'s `prefix_op`), sign is never
part of `INTEGER_LITERAL`.

Given that, the range check itself splits cleanly across two phases by what each phase can
actually see:

- **At lexing:** check the literal's magnitude fits in 32 bits at all — i.e. reject
  anything past `u32::MAX` (4294967295), regardless of suffix. This bound holds no matter
  what precedes the literal; no unary minus makes a 40-digit number fit in 32 bits. Safe
  and unambiguous with zero context.
- **At parsing:** check the tighter, sign-aware bound — `i32::MAX` (2147483647) for an
  unsuffixed literal not immediately preceded by a unary minus, or the full `i32` range
  (down to `i32::MIN`) when the AST shape is `Negate(IntLiteral n)`. This needs the
  surrounding syntax the lexer doesn't have.

Considered and rejected as the simpler alternative: skip the lexer-side check entirely and
let sema/parsing own the whole thing (this is what rustc does — the lexer just captures
digits). Simpler, one place instead of two, and not meaningfully slower since sema already
walks the full AST regardless. Still leaning toward the split above instead, since the
lexer-side bound is free, unambiguous, and catches the pathological case (absurdly long
digit runs) at the earliest possible point — but this is genuinely a toss-up, not a strong
conviction either way.

## History

- v0.1.0: syntax proposed (default `i32`, `u` suffix for `u32`); range-checking
  recommendation written down for discussion, not yet decided.
- v0.1.0: noted the `i32::MIN` range-check gotcha (bare literal magnitude one past
  `i32::MAX`, only valid combined with a preceding unary minus) for whenever range-checking
  is implemented.
- v0.1.0: rejected folding sign into the lexer's literal token (ambiguous — the same "`-`
  followed by digits" shape means either a signed literal or binary subtraction depending
  on the *previous* token, which a forward-only lexer can't see); settled on splitting the
  range check itself instead — a sign-independent `u32`-magnitude bound at lexing, the
  tighter `i32`-aware bound (with the `i32::MIN` exception) at parsing.
