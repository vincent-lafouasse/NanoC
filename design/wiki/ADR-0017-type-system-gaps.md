# ADR-0017: Type system gaps: pointer arithmetic, shadowing

**Status:** Open
**Area:** Language Design

Two small but distinct gaps in the type checker's design, grouped here because they're
each short and each blocks finalizing the type checker, without yet being developed enough
to deserve separate files. Split out individually if either grows substantially — a third
gap that used to live here, the cast operator, already did: see
[cast-operator](ADR-0021-cast-operator.md).

## Pointer arithmetic rules

`ptr` is currently an opaque, untyped pointer; typed pointers (`i32*`, `Point*`) also
exist. The rules for mixing them in arithmetic aren't yet defined:

- `i32* + i32` — does the compiler scale the offset by `sizeof(i32)`, or is it byte-level?
- `ptr + i32` — valid at all? `ptr` has no element size to scale by.
- `i32* - i32*` — valid? What's the result type?
- Mixing `ptr` and `i32*` in arithmetic or assignment — valid?

These need design decisions before the type checker can enforce anything here. This also
blocks the header-behind-pointer optimization noted in
[stdlib-strings](ADR-0016-stdlib-strings.md).

## Variable shadowing

Whether declaring a variable with the same name as one in an outer scope is a hard error,
a warning, or silently allowed isn't decided. Many systems languages allow it (Rust, C),
with optional warnings; others forbid it outright.

## History

- Predates version tracking — carried over from the original design document, under
  Semantic Analysis's "Uncertain checks." Split into its own wiki file as part of
  separating the Tour's settled reference material from open design questions.
- v0.1.0: cast operator split out into its own file
  ([ADR-0021](ADR-0021-cast-operator.md)) once it turned out to be blocking several
  independent things (integer/char literal typing, the stdlib-strings optimization) —
  substantial enough on its own to no longer fit "short, undeveloped" alongside these two.
