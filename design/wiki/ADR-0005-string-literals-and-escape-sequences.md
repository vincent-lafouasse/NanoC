# ADR-0005: String literals and escape sequences

**Status:** Settled (octal deferred — see below)
**Area:** Language Design

## String literal termination and typing

**Decision:** string literals are always null-terminated. Kept as `ptr` for now — this
matches C's `char *` exactly, so `extern`-declared functions (see
[module-system-and-ffi](ADR-0013-module-system-and-ffi.md)) can take a NanoC string literal
directly, no conversion needed.

Whether literals should be typed `u8*` instead of bare `ptr` is still open, but no longer
blocking — termination was the part that actually needed deciding before FFI could work at
all.

A richer, length-carrying string type is explicitly not part of the core language or the
literal representation — see [stdlib-strings](ADR-0016-stdlib-strings.md).

## Escape sequences

| Escape | Meaning | Escape | Meaning |
|---|---|---|---|
| `\n` | newline | `\a` | alert / bell |
| `\t` | tab | `\b` | backspace |
| `\r` | carriage return | `\f` | form feed |
| `\\` | backslash | `\v` | vertical tab |
| `\"` | double quote | `\xHH` | byte value, exactly 2 hex digits |
| `\0` | null byte | `\dDDD` | byte value, exactly 3 decimal digits (000-255) |
| `\e` | ESC (`0x1B`) | | |

`\0` is its own dedicated escape, not the degenerate case of a general octal scheme —
NanoC has no octal escapes at all (see below). `\xHH` and `\dDDD` both require a fixed
digit count — exactly 2 hex digits, exactly 3 decimal digits — unlike C's `\x`, which
greedily consumes an unbounded run of hex digits (a well-known footgun where a stray
hex-looking character after the intended escape gets silently absorbed into it). A
`\dDDD` value outside 000-255, same as a malformed `\xHH`, is a `MalformedEscapeSequence`
error — no new error kind needed, same "syntactically escape-shaped, semantically invalid"
bucket.

`\e` is a GNU/Clang extension, not standard C — but unlike trigraphs (below) it isn't
legacy cruft, it's the one escape a systems language writing directly to terminals (ANSI
color codes, cursor control) plausibly wants that the rest of the table doesn't cover.
Kept for the mnemonic even though `\d027` says the same thing, the same reasoning that
keeps `\n` around instead of forcing `\d010` everywhere.

### No octal escapes (`\ooo`) — deferred, not decided

Not a rejection — there's simply no reason to add it right now. C's octal escape is 1-3
digits, greedily consumed, so `\1234` means octal `\123` followed by a literal `4` — a
context-dependent, ambiguous parse that's exactly the category of thing this grammar
rejects elsewhere (mandatory braces, no `i[arr]`, "unambiguous and context-free by
construction"), and `\xHH`/`\dDDD` already cover "arbitrary byte value" with a fixed,
unambiguous width. So bare `\ooo` specifically stays out.

If octal is reintroduced later, it would use the same marker convention as `\xHH`/`\dDDD`:
a fixed-format `\oOOO` (explicit `o`, exactly 3 octal digits) — never bare digits, for the
same collision reason `\dDDD` needs its marker (see below).

### Why `\dDDD` needs a marker, unlike a naive `\DDD`

An unmarked digit escape would collide with C's octal convention — `\101` reads as `'A'`
(octal 101 = 65) to anyone with C muscle memory, but would be byte 101 (`'e'`) under
decimal-by-default. That's not a missing feature, it's a silent wrong-value bug: no error,
no warning, just the wrong byte in a real program. Requiring the `d` marker (mirroring `x`
for hex) means bare digits after a backslash stay meaningless — rejected as
`UnknownEscapeSequence`, loud and immediate — rather than quietly reinterpreted under
different rules than a C programmer would expect. OCaml hit the identical problem and
solved it the identical way: its own octal escape is `\oOOO` (explicit `o` marker)
specifically so undecorated `\DDD` could safely mean decimal without colliding with
anyone's C-octal intuition.

### No trigraphs, no `\?`

Trigraphs (`??=`, `??/`, etc. — three-character sequences ISO 646 keyboards used to type
punctuation absent from their character sets) were never under consideration for NanoC —
not a deliberate rejection, just never on the table to begin with. `\?` exists in C purely
as a defense against a source string's `?` characters accidentally forming a trigraph;
with no trigraph processing, `\?` has no job to do, so it isn't an escape sequence at all —
`?` is just an ordinary printable character, no escaping needed.

Trigraphs specifically reflect a general policy, not a one-off: NanoC's C-likeness is
about syntax familiarity and target-audience fit, not an obligation to reproduce every
historical C mechanism. A legacy feature carried into C for compatibility with 1960s-70s
hardware and character sets earns a place in NanoC only if it still pulls its weight for
NanoC's own goals — trigraphs don't, unconditionally. Octal is a different case: it isn't
ruled out on principle, there's just no pull for it yet (see above).

### Deferred, not rejected

- `\'` (escaped single quote) — no purpose today since NanoC has no character-literal
  syntax yet (a `'` inside a double-quoted string needs no escaping at all). Add it
  alongside char literals if/when they're implemented.
- `\uXXXX`/`\UXXXXXXXX` (Unicode code point escapes) — out of scope while nothing else in
  the type system is Unicode-aware; string literals are plain byte sequences.

## History

- v0.1.0: null-termination decided, full escape table (including `\dDDD` and `\e`)
  settled. Trigraphs ruled out unconditionally; octal corrected from "ruled out" to
  "deferred" — no reason to add it now, but `\oOOO` (marker-based, matching `\xHH`/`\dDDD`)
  is the reintroduction path if it's ever needed.
