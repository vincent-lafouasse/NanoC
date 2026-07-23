# Changelog

All notable changes to the NanoC language design are recorded here, one entry per
version. See `design/wiki/` for the in-progress discussion behind any of these decisions, and
`design/book/` ("A Tour of NanoC") for the current state of the settled design.

This tracks the *design*, not implementation progress — the compiler itself (currently an
OCaml lexer, under `NanoC/`) is a separate, unversioned, in-progress effort.

## [0.2.0] - 2026-07-24

Integer literals (`design/wiki/ADR-0019-integer-literals.md`) went from "syntax settled,
everything else open" to a mostly-settled design, developed alongside bringing the OCaml
lexer's literal handling to MVP completion.

### Added

- Full-word integer suffixes for every primitive: `u32`, `u8`, `i32` (the last purely for
  symmetry — bare digits already default to `i32`), with the existing `u` kept as
  shorthand for `u32`. Answers the previously-open "how does a `u8`-typed literal get
  written?" question directly (`42u8`).
- Negating an unsigned or byte literal (`-42u32`, `-1u8`) is settled as a hard error,
  diverging from C (which allows it and wraps) — no stated use case, and no escape hatch
  removed by disallowing it.
- Integer-literal range checking split across compiler phases: `u32`/`u8`-suffixed
  literals are fully bounds-checked at lexing (nothing about their validity depends on a
  preceding sign, now that negating them is an error); a bare/`i32`-suffixed literal is
  checked up to `abs(i32::MIN)` at lexing, with the single leftover ambiguous value
  (`i32::MIN` itself) resolved by the parser, which already needs to fold a preceding
  unary minus for that case regardless.
- A `ptr` suffix for magic/hardware addresses (`0x40020014ptr`) proposed and deferred —
  not useful for the current RISC-V/xv6 target, but motivated by a future bare-metal ARM
  port.

### Changed

- Corrected an earlier, unexamined guess in ADR-0019: a not-quite-matching integer-suffix
  tail (`42u16`, `42u3`) is not a malformed-suffix lex error — it's just two individually
  valid, adjacent tokens, with any resulting invalid-expression-shape left to the parser to
  reject. The wrong guess is kept in the ADR's text rather than silently deleted, with a
  note on why it was corrected.

### Implementation

- The OCaml lexer (`lib/lexer.ml`) reached MVP completion: every keyword, operator, and
  punctuation token currently in the language, plus all four literal kinds (string, char,
  int/unsigned/byte/ptr) with their full escape tables and the range-checking split above.
  Covered by unit tests (`test/test_lexer.ml`), a standalone `int64`-bounds-checking helper
  and its tests (`lib/atoi.ml`, `test/test_atoi.ml`), and whole-program integration tests
  (`test/test_lexer_integration.ml`).

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

- Design doc split: settled reference moved into "A Tour of NanoC" (`design/book/`); open
  questions, proposals, and post-MVP plans moved into a one-file-per-topic design wiki
  (`design/wiki/`).
- Renamed from "NanoC Design Document" to "A Tour of NanoC".
