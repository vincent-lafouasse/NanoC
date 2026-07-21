# ADR-0003: Assignment as expression

**Status:** Open
**Area:** Language Design
**Leaning:** statement only

## Context

In C, assignment is an expression that returns the assigned value, enabling chained
assignment (`a = b = 0`) and assignment inside conditions (`while ((n = read()) > 0)`).
Whether NanoC should follow that, or make assignment statement-only, is the question.

## Option A — statement only (current leaning)

```nanoc
x = value;          // valid as a statement
var y = x = 0;      // compile error
if (x = read()) {}  // compile error
```

- Eliminates the classic `if (x = 0)` bug class entirely, structurally — not via a
  warning, by construction.
- Simpler grammar: assignment is its own statement form, not an operator.
- The `Precedence::Assignment` slot in the Pratt parser's table is still useful as a floor
  when parsing the right-hand side of an assignment statement, even without assignment
  being a general expression.

## Option B — expression (C-style)

```nanoc
a = b = 0;                    // right-associative, both set to 0
while ((n = read()) > 0) {}   // assign and test in one
```

- More expressive, familiar to C programmers.
- `a = b = 0` maps naturally onto the Pratt parser (right-associative, same precedence
  for the recursive call) — this would not be hard to implement.
- Risk: assignment-in-condition is a well-known, common source of bugs.

## Current leaning

Statement only, confirmed. Too footgunny when there are simple alternatives — the bug
prevention outweighs the expressiveness, and NanoC already has `goto` available for the
loop-and-test patterns that would otherwise motivate assignment-as-expression (see
[statement-oriented-expressions](ADR-0009-statement-oriented-expressions.md) for the
broader principle this fits into).

## History

- Predates version tracking — carried over from the original design document. Leaning
  toward "statement only" has been stable since the document's earliest drafts, and
  reconfirmed since.
