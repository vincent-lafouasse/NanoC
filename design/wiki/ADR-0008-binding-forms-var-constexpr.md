# ADR-0008: Binding forms: `var` / `constexpr`

**Status:** Open
**Area:** Language Design
**Leaning:** toward it, not fully sold

## Proposal

`const` would be removed. Two binding forms only:

```nanoc
var       x: i32 = compute();  // mutable, stored, addressable — the only runtime binding
constexpr x: i32 = 67;         // folded, NOT stored, NOT addressable — compile-time only
```

The reasoning behind dropping runtime `const`: immutability of runtime variables would
stay a programmer convention, not a compiler-enforced property, on the theory that there's
nothing at the chip level corresponding to read-only registers or stack slots — enforcing
it in the compiler would add complexity for a guarantee that evaporates at the asm level
anyway. Leaning toward this, but not completely sold — it's a real trade-off (giving up a
genuinely useful compiler-checked guarantee purely because the hardware doesn't have an
equivalent), not an obviously free win, and deserves more thought before it's called
settled.

`constexpr` would be a value property — the compiler evaluates the initializer at compile
time and substitutes the result at every use site, no storage emitted. Taking the address
of a `constexpr` binding would be a sema error (nothing to point at).

A `constexpr` initializer would need to be a *constexpr expression*: a literal, another
`constexpr` binding, or a call to a `comptime fn` with `constexpr` arguments — sema-checked.
See [compile-time-conditionals](ADR-0015-compile-time-conditionals.md) for `comptime fn`.

`comptime` and `inline` (see [inline-keyword](ADR-0007-inline-keyword.md)) would be
orthogonal:

| specifier | compile-time | runtime |
|---|---|---|
| `comptime fn` | evaluated by compiler | regular call |
| `inline fn` | — | always inlined |
| `comptime inline fn` | evaluated by compiler | always inlined |

## History

- Predates version tracking — carried over from the original design document. Originally
  recorded as an already-settled decision; corrected — proposed and in active
  consideration, leaning toward this design, but not settled.
