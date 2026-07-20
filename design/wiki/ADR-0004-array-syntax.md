# ADR-0004: Array syntax

**Status:** Open
**Area:** Language Design

## Context

Static arrays are anticipated in the grammar but not implemented. Two questions: what the
declaration syntax looks like, and whether arrays are stack-allocated only.

## Options

- `i32[10]` — postfix, matches how pointer types are written (`i32*`), so array-ness reads
  as another postfix type modifier alongside pointer-ness.
- `[10]i32` — prefix, closer to Go's array syntax.

## Current leaning

`i32[10]` (postfix), for consistency with pointer syntax — `i32*` and `i32[10]` both read
left-to-right as "start from `i32`, modify it."

Whether arrays are stack-allocated only, or can appear elsewhere (struct fields, globals),
isn't resolved yet either.

## History

- Predates version tracking — carried over from the original design document, not yet
  implemented or fully settled.
