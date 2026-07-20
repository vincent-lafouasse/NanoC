# Changelog

All notable changes to the NanoC language design are recorded here, one entry per
version. See `aux/wiki/` for the in-progress discussion behind any of these decisions, and
`aux/book/` ("A Tour of NanoC") for the current state of the settled design.

This tracks the *design*, not implementation progress — the compiler itself (currently an
OCaml lexer, under `NanoC/`) is a separate, unversioned, in-progress effort.

## [0.1.0] - 2026-07-20

First versioned snapshot of the design. Represents everything settled in the original
design document plus this session's additions, restructured into "A Tour of NanoC"
(settled reference) and a design wiki (open questions, proposals, post-MVP plans), split
one file per topic instead of a handful of growing, mixed-status documents.

### Added

- String literal encoding settled: always null-terminated, kept as `ptr` — enables direct
  `extern`/FFI interop with no conversion step.
- Full escape sequence table: `\n \t \r \\ \" \0 \a \b \f \v`, `\xHH` (fixed 2 hex digits),
  `\dDDD` (fixed 3 decimal digits, explicitly marked to avoid colliding with C's octal
  convention), `\e` (ESC). Octal escape sequences are still on the table, just very de-prioritized.
- Explicit non-support recorded, with rationale: no octal escapes (`\ooo`), no trigraphs,
  no `\?`.
- A length-carrying `Str` stdlib type proposed for when a richer string type is needed —
  kept out of the core language and the literal representation on purpose.

### Carried over from the original design document (predates version tracking)

- Core language: `var`/`constexpr` as the only two binding forms (no runtime `const`),
  mandatory initialization, declarations-before-statements, mandatory braces, no
  `break`/`continue`, expression-statements restricted to calls/syscalls, statement-
  oriented (no block/if expressions), structs never passed by value, `i[arr]` disallowed.
- A defined-vs-undefined-behavior table diverging from C where it costs nothing to (signed
  overflow wraps, arithmetic right shift, left-shift of negatives defined, left-to-right
  evaluation order), while keeping UB where a check would add real runtime cost (division
  by zero, out-of-bounds access, null dereference — all UB, as in C).
- Compiler architecture: lexer/parser/sema fallible with spans, codegen infallible (panics
  only on internal bugs, never on user source), fail-fast error reporting (no recovery).
- Two-pass semantic analysis (declaration collection, then resolution/checking).

### Documentation

- Design doc split: settled reference moved into "A Tour of NanoC" (`aux/book/`); open
  questions, proposals, and post-MVP plans moved into a one-file-per-topic design wiki
  (`aux/wiki/`).
- Renamed from "NanoC Design Document" to "A Tour of NanoC".
