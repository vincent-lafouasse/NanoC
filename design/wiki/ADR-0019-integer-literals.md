# ADR-0019: Integer literals

**Status:** Open
**Area:** Language Design
**Leaning:** syntax settled (i32 default, u suffix); range-check hard-error recommended, not confirmed

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

## History

- v0.1.0: syntax proposed (default `i32`, `u` suffix for `u32`); range-checking
  recommendation written down for discussion, not yet decided.
- v0.1.0: noted the `i32::MIN` range-check gotcha (bare literal magnitude one past
  `i32::MAX`, only valid combined with a preceding unary minus) for whenever range-checking
  is implemented.
