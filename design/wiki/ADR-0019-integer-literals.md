# ADR-0019: Integer literals

**Status:** Open
**Area:** Language Design
**Leaning:** syntax mostly settled (i32 default; u/u32/u8/i32 suffixes, u short for u32); three separate token constructors (`IntLiteral`/`UnsignedIntLiteral`/`ByteLiteral`), not a tagged payload; negating an unsigned or byte literal is a hard error; range-check hard-error recommended, not confirmed — `UnsignedIntLiteral`/`ByteLiteral` fully bounds-checked at lexing, `IntLiteral` checked up to `abs(i32::MIN)` at lexing with the single leftover case resolved by the parser; a `ptr` suffix for magic addresses deferred until a bare-metal ARM target exists

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

**Leaning:** full-word suffixes for every primitive — `u32`, `u8`, and `i32` (the last
purely for symmetry; bare digits already default to `i32`) — with the existing `u` suffix
kept as shorthand for `u32` rather than removed. Answers this question directly: `42u8`.

**Settled, correcting an earlier guess here:** a not-quite-matching tail like `42u3` or
`42u16` is *not* a malformed-suffix error — it's just two separate, individually valid
tokens (`UnsignedIntLiteral 42` via the `u` shorthand, then `IntLiteral 3`/`16`), same as
any other two adjacent tokens with no whitespace between them (`42+7` needs none either).
There's no `u16` type, so "merged" was never a competing valid reading the lexer needed to
disambiguate away — unlike `==` vs. `=;=`, where both readings really are individually
valid and maximal munch has to pick one. Two adjacent atoms with no operator between them
*is* invalid, but as an expression — that's the parser's job to reject (no grammar
production for `atom atom`), not the lexer's, exactly the same phase separation already
used for sign-combining around `i32::MIN` below. (An earlier draft of this ADR guessed the
opposite — "malformed-suffix error, presumably" — before the suffix set was implemented
and checked against actual behavior; corrected once it was.)

### Token representation: three constructors, not a tagged payload

Considered: a single `IntLiteral of { magnitude : int; suffix : suffix }` with
`suffix = NoSuffix | I32 | U32 | U8`, one constructor for every int literal regardless of
suffix. Rejected in favor of three separate `Token.kind` constructors —
`IntLiteral of int` (i32, default or explicit `i32` suffix — behaviorally identical, so no
separate case needed for the explicit spelling), `UnsignedIntLiteral of int` (u32), and
`ByteLiteral of int` (u8) — because the three behave differently enough (see negation,
below) that a tagged payload would just push a `match suffix with` into every place that
needs to branch on it, with no exhaustiveness check forcing every case to be handled. Three
constructors make each branch-point a real pattern match instead. The `u`/`u32` spelling
distinction never reaches the token either way — both produce `UnsignedIntLiteral`, since
sema only ever needs to know the resulting type, not which spelling was written.

### Negating an unsigned or byte literal is a hard error

`Negate(IntLiteral n)` is legal (this is the `i32::MIN` case, below). `Negate
(UnsignedIntLiteral n)` and `Negate(ByteLiteral n)` are hard errors, not "legal but
unusual" (unlike C, which allows negating an unsigned value — it just wraps). No stated
use case for it, and no syntax is being removed to enforce it — anyone who wants a specific
unsigned bit pattern that happens to look like a negative number's two's complement can
just write that pattern directly (bitwise, or hex once that exists) instead of negating a
literal. This is also the concrete payoff of picking separate constructors over a tagged
payload above: `Negate`'s operand match has three constructor patterns, not one, so the two
rejecting arms have to be written explicitly rather than falling out of a missed `if`.

### Deferred — a `ptr` suffix for magic addresses (`0x40020014ptr`)

Motivating case: fixed hardware register addresses in bare-metal/embedded code (e.g. GPIO
registers on an STM32F4) — standard C practice is
`(volatile uint32_t *)0x40020014`-style casts for exactly this. Not useful for the current
target ([RISC-V and xv6](../book/src/intro.md) — chosen to close the design space for the
MVP, not a permanent restriction) since hosted xv6 user programs get pointers from the
kernel and never need to name a physical address directly; the case only exists for
kernel-level or bare-metal code, which is squarely the shape of thing a future ARM port
(e.g. bare-metal on an STM32F4) would need, and one is on the table "way after MVP."

Cheaper than it first looks whenever it does happen: a `ptr` literal is a compile-time bit
reinterpretation of a known constant — zero runtime cost, always well-defined — consistent
with the "well-defined where it's free" thread running through the Tour's Defined Behavior
table (signed overflow wraps, left-shift of negative is bitwise, etc.). It also doesn't
need the general cast operator ([ADR-0021](ADR-0021-cast-operator.md)) to land first,
since it only answers "how do I write a specific known address as `ptr`," not "how do I
convert an arbitrary `i32` expression to `ptr`" — narrower than a real cast, and would share
the same magnitude-bound check as the `u32`/`u8` suffixes above. Deferred, not rejected —
revisit once the ARM port is actually on the table, not before.

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

Given the three-constructor split above, the range check itself splits cleanly by what
each token kind and each phase can actually see:

- **`UnsignedIntLiteral`/`ByteLiteral`, fully checked at lexing.** Since negating either is
  now a hard error, their magnitude is never combined with a sign — nothing about their
  validity depends on surrounding syntax. The lexer can and should fully own these: reject
  anything past `u32::MAX` (4294967295) for `UnsignedIntLiteral`, past `u8::MAX` (255) for
  `ByteLiteral`. No later phase needs to touch it.
- **`IntLiteral`, mostly checked at lexing, one value deferred to the parser.** The
  magnitude alone can't exceed `abs(i32::MIN)` = 2147483648 under *any* interpretation,
  negated or not — the lexer rejects anything past that unconditionally, narrower than the
  original `u32::MAX` guess since this constructor can never carry a `u`/`u8` suffix. That
  leaves exactly one ambiguous magnitude, not a range: `2147483648` itself, valid only as
  `i32::MIN` (i.e. only when immediately negated), invalid as a bare positive `i32`. The
  parser is what resolves it, not sema — the parser is already the phase that folds the
  involutive `Negate(IntLiteral n)` shape (needed for `i32::MIN` regardless), so it already
  holds the one fact needed ("was this literal immediately preceded by a unary minus") to
  decide this one leftover case without waiting for a separate pass.

Considered and rejected as the simpler alternative: skip the lexer-side check entirely and
let the parser/sema own the whole thing (this is what rustc does — the lexer just captures
digits). Simpler, one place instead of two, and not meaningfully slower since the parser
already walks every literal regardless. Still leaning toward the split above instead, since
two of the three literal kinds end up lexer-complete for free once negation of
unsigned/byte literals is a hard error, and the third only has a single leftover value
instead of an entire range to defer.

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
- v0.1.0: settled on three separate token constructors (`IntLiteral`/`UnsignedIntLiteral`/
  `ByteLiteral`) over a single tagged-payload constructor, driven by negating an unsigned
  or byte literal becoming a hard error (unlike C) — separate constructors make the
  rejecting match arms exhaustiveness-checked rather than an easily-missed `if` on a tag.
  This also sharpened the range-check split: `UnsignedIntLiteral`/`ByteLiteral` are now
  fully checked at lexing (nothing about their validity ever depends on a preceding sign),
  and `IntLiteral`'s lexer-side bound tightens from `u32::MAX` to `abs(i32::MIN)`
  (2147483648), leaving only one exact leftover value — not a range — for the parser to
  resolve, using the same involutive-`Negate`-folding step it already needs for `i32::MIN`.
- v0.1.0: corrected the earlier "malformed-suffix error, presumably" guess for `42u3`/
  `42u16` — implemented and checked against actual lexer behavior, it turned out to be an
  unexamined assumption rather than a real ambiguity. Settled as two separate, individually
  valid tokens instead, with the invalid-as-an-expression consequence left to the parser.
