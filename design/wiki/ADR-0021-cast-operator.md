# ADR-0021: Cast operator

**Status:** Open
**Area:** Language Design

## Context

NanoC currently has no explicit cast operator. Combined with strict no-implicit-conversion
(the Tour's Language Design chapter: "The RHS expression type must exactly match the
declared type of the LHS. No implicit conversions."), this means there is currently no way
to mix or convert between integer types at all — not even deliberately.

Concretely, this already blocks several things that have come up independently:

- **Is `i32 + u8` valid?** With no implicit promotion and no cast, no — you can't mix
  types in an expression under any circumstances right now, which is plausibly too
  restrictive for ordinary code (any function taking a `u8` byte and doing arithmetic
  against an `i32` index, say).
- **Is `var x: i32 = some_u32` a type error?** Yes, under strict no-implicit-conversion —
  with no escape hatch, there's no way to explicitly say "yes, I mean to reinterpret or
  truncate this."
- **How does a `u8`-typed integer or char literal get written?** (see
  [integer-literals](ADR-0019-integer-literals.md) and
  [character-literals](ADR-0020-character-literals.md)) A bare integer literal defaults to
  `i32`; without a cast, there's no stated way to narrow it to `u8` at the point of use.
- **The stdlib strings header-behind-pointer optimization** (see
  [stdlib-strings](ADR-0016-stdlib-strings.md)) needs to read a `u32` length from raw
  bytes at `p - 4`, which needs both pointer arithmetic (see
  [type-system-gaps](ADR-0017-type-system-gaps.md)) and a cast to reinterpret the read.

Without a cast, users have no escape hatch at all for any of these — a cast operator
should be designed before the type checker is finalized.

## Options

- **`x as T`** — keyword-based, Rust-style. Reads left-to-right, unambiguous, easy to
  `grep` for every cast in a codebase.
- **`(T)x`** — C-style prefix cast. Most familiar to the target audience, but reuses `(`
  `)` in a way that adds a second meaning to parentheses (grouping vs. casting) — the
  parser needs to disambiguate `(i32)` (a cast applied to what follows) from `(i32 + 1)`
  (a grouped expression), which C itself resolves via a lookahead rule that's a known
  source of parser complexity.

## Open sub-questions

- Truncating vs. reinterpreting casts — does `(u8) some_i32_value` truncate the low byte
  (defined, cheap), or is out-of-range behavior UB? Given the project's general stance
  (well-defined where it's free, UB where a check costs something — see the Tour's
  Defined Behavior table), truncation reads like the natural answer, but not yet decided.
- Pointer casts — does the same operator handle `ptr` ↔ typed-pointer conversions, or is
  that a separate mechanism? Directly relevant to
  [type-system-gaps](ADR-0017-type-system-gaps.md)'s pointer arithmetic questions.
- Does a cast ever fail at runtime (and if so, how — UB, or an error), or is it always a
  reinterpretation that cannot fail by construction?

## History

- Predates version tracking as a bullet point inside
  [type-system-gaps](ADR-0017-type-system-gaps.md) ("Cast operator"). Split into its own
  file once several independent things (integer/char literal typing, the stdlib-strings
  optimization) all turned out to be blocked on the same missing piece.
