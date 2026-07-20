# ADR-0016: Stdlib strings

**Status:** Settled (design); implementation deferred (post-MVP)
**Area:** Future Work

**Not currently implemented** — and not worth pursuing before an MVP compiler (parser,
sema, codegen) exists, since none of this matters until codegen can emit `.rodata` layouts.

**Core language commitment:** string literals are `ptr`, always null-terminated (see
[string-literals-and-escape-sequences](ADR-0005-string-literals-and-escape-sequences.md)). That
guarantee is deliberately minimal — enough for `extern` FFI (see
[module-system-and-ffi](ADR-0013-module-system-and-ffi.md)), nothing richer.

## Decision

A length-carrying string type lives in the standard library, not the language:

```nanoc
struct Str {
    data: ptr,
    len: u32,
}
```

Always passed as `Str*` — not a special case, an ordinary struct already gets this for
free under the existing struct-by-value ban. String literals can back a `Str` at zero
runtime cost: the compiler emits a static `Str` instance in `.rodata` per `Str`-typed
literal, pointing at the (null-terminated) bytes with `len` filled in — no allocation.
Stdlib supplies the accessors (`str_len`, `str_data`, etc.).

## Deferred optimization — header-behind-pointer

An alternative keeps a string as a single `ptr` (no wrapper struct) by storing the length
just *before* the pointed-to data, so `p[0]` is still the first byte and length is
`*(u32*)(p - 4)`. Not rejected, just blocked: it needs pointer arithmetic and a cast
operator, neither of which exist yet (see [type-system-gaps](ADR-0017-type-system-gaps.md)). Worth
revisiting once those land, if the extra word per `Str` ever actually matters.

## History

- Predates version tracking — carried over from the original design document as an
  already-settled design, with implementation deferred post-MVP.
