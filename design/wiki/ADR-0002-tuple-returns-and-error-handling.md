# ADR-0002: Tuple returns and error handling

**Status:** Open
**Area:** Language Design

## Context

RISC-V's calling convention provides two return registers (`a0`, `a1`); most functions
only use `a0`. This is about whether and how NanoC exposes the second register for richer
return semantics, and whether that ties into an error-handling story.

## Proposal A — simple tuples (minimal)

```nanoc
fn divmod(a: i32, b: i32) -> (i32, i32) {
    return (a / b, a % b);
}

var quot: i32;
var rem: i32;
(quot, rem) = divmod(10, 3);
```

- Pro: minimal syntax, explicit, maps directly to `a0`/`a1`.
- Con: doesn't address error-handling ergonomics on its own.

## Proposal B — tuple-based Result convention

```nanoc
// Convention: (value, error_code) — a0 = value (or garbage if error), a1 = error code
fn divide(a: i32, b: i32) -> (i32, i32) {
    if (b == 0) {
        return (0, 1);  // ERR_DIV_ZERO
    }
    return (a / b, 0);
}

var result: i32;
var err: i32;
(result, err) = divide(10, 0);
if (err != 0) {
    // handle error
}
```

- Pro: uses both registers, explicit error handling.
- Con: very verbose for error propagation — every call site needs the full unpack-and-check
  dance, with no way to shorten it.

## Proposal C — Zig-style `try`

```nanoc
fn process_file() -> (i32, i32) {
    const fd = try open_file("data.txt");  // auto-propagate on error
    const data = try read_data(fd);
    const result = try process(data);
    return (result, 0);
}

// `try` desugars to:
// var fd: i32;
// var err: i32;
// (fd, err) = open_file("data.txt");
// if (err != 0) { return (undefined, err); }
```

- Pro: ergonomic, explicit keyword, makes the Result convention actually practical —
  proven in Zig.
- Con: adds a keyword, and introduces implicit control flow (early return) — a real
  tension with the "no implicit control flow" thread running through the rest of the
  language design.

## Key insight

Without `try`-style propagation, the Result convention (Proposal B) is too verbose to be
worth using in practice. It's effectively an all-or-nothing decision: either commit to
`try`, or stick with plain tuple returns (Proposal A) and let error codes be handled with
ordinary `if` checks, C-style.

## Current leaning

No real leaning yet. Proposal A (simple tuples) reads as the most appealing on its face,
but adopting it isn't as small a step as it looks — it would mean committing to
tuples/pairs as an actual language construct, which is a bigger decision than "how do we
use the second return register" and hasn't been thought through on its own merits.

## History

- Predates version tracking — carried over from the original design document. Previously
  recorded as leaning toward Proposal A; corrected — no leaning either way yet, since
  Proposal A's real cost (introducing tuples as a language construct) hadn't been weighed.
