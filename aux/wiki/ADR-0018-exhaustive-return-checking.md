# ADR-0018: Exhaustive return checking

**Status:** Deferred
**Area:** Implementation

## Context

If `fn foo()` with no `->` means "returns unit" (current leaning: yes, an omitted return
type means unit), must every code path in a non-unit function end with a `return`?

This requires control-flow analysis — building a CFG and checking that all exit paths
return. Many compilers implement this as a warning rather than a hard error, or defer it
entirely. The check is non-trivial in the presence of `goto`, since a jump can route
control flow in ways a purely structural (non-CFG) walk of the AST would miss.

## Status

Deferred until post-MVP — not needed before there's a working compiler capable of
building a CFG for other reasons (codegen, later optimization passes) anyway.

## History

- Predates version tracking — carried over from the original design document, under
  Semantic Analysis's "Uncertain checks." Split into its own wiki file as part of
  separating the Tour's settled reference material from open design questions.
