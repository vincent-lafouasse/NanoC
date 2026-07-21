# ADR-0011: Optimization passes and debug info

**Status:** Deferred
**Area:** Implementation
**Leaning:** debug info — yes, eventually; optimization passes — maybe never

## Optimization passes

Add any (dead code elimination, constant folding, ...)? **Leaning: maybe none, ever** —
not just "not yet." Keeping codegen simple and predictable is close to the point of the
whole project (see the Tour's Compiler Architecture chapter, which describes codegen as a
deterministic, infallible translation once sema passes); optimization passes are exactly
the kind of thing that complicates that story permanently, not just during MVP.

## Debug info

Generate DWARF? **Leaning: yes, but way later.** Unlike optimization passes, this isn't in
tension with the "1:1 mapping to assembly" philosophy — DWARF describes the mapping
between source and the assembly already being generated, it doesn't change what gets
generated. Wanted eventually; just far from urgent.

## History

- Predates version tracking — carried over from the original design document. Originally
  recorded uniformly as "deferred, not MVP" for both; corrected — debug info leans toward
  yes eventually, optimization passes leans toward maybe never.
