# ADR-0010: Error recovery

**Status:** Open
**Area:** Implementation
**Leaning:** fail-fast — a velocity call, may revisit

## Context

Should the parser (and lexer) attempt to continue after hitting an error, in order to
report multiple errors in one pass?

## Options

- **Fail fast** — report the first error only, stop. Simpler parser, but a user fixing one
  error only finds out about the next one on the following compile.
- **Recover and continue** — report multiple errors per compile. More useful for the user,
  meaningfully more complex parser (needs resynchronization points — where does parsing
  resume after a syntax error?).

## Current status

Fail fast — a velocity call for this stage of the project, not a permanent philosophical
position. Multi-error recovery is real, useful work (resynchronization points, etc.) that
can wait; right now progress matters more than diagnostic ergonomics, so the simpler
option wins by default. Open to reconsidering once the compiler is far enough along that
"only one error per run" starts costing more than it saves.

This is reflected directly in the OCaml lexer's implementation today: `Result` values
short-circuit on the first error, and `Lexer.tokenize`'s driving loop stops as soon as
`next_token` returns an `Error`, with no lexer state threaded through the error path to
resume from (see the lexer's error type design, which deliberately doesn't carry a
resumable position).

## History

- Predates version tracking — carried over from the original design document. The OCaml
  port's lexer error handling was built consistent with this decision from the start.
  Clarified: fail-fast is a deliberate velocity trade-off for the current stage, explicitly
  open to reconsideration later, not a closed decision.
