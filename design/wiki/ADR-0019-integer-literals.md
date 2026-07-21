# ADR-0019: Integer literals

**Status:** Open — syntax settled, range checking not yet confirmed
**Area:** Language Design

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

## History

- v0.1.0: syntax proposed (default `i32`, `u` suffix for `u32`); range-checking
  recommendation written down for discussion, not yet decided.
