# ADR-0007: `inline` keyword

**Status:** Open
**Area:** Language Design
**Leaning:** none at all — deep, unexplored consequences

Being fully specified below is not a sign this is close to decided. There's genuinely no
leaning either way yet — the semantics below have deep, not-yet-explored consequences
(mandatory inlining that must fail loudly rather than degrade, interaction with taking a
function's address, what "the compiler must inline it or error" even means for very large
or awkward call graphs), and being written out in full detail is what made those
consequences visible in the first place, not evidence the proposal has been vetted.

## Proposal

```nanoc
inline fn square(x: i32) -> i32 {
    return x * x;
}

fn compute() -> i32 {
    return square(5);  // always inlined
}
```

## Semantics

- The `inline` keyword guarantees the function is inlined at every call site.
- Not a hint or optimization suggestion — a mandate. The compiler must inline it or fail
  with an error if inlining is impossible.
- Recursive calls to an `inline` function are a compile error (inlining a recursive call
  would not terminate).
- Taking the address of an `inline` function is a compile error — an inlined function has
  no stable address.

## Rationale

- Deterministic performance characteristics: no call overhead, no ambiguity about whether
  the compiler decided to inline or not.
- Predictable generated assembly for users who need it — fits the "1:1 mapping to
  assembly" philosophy: what you write is what you get, including this.
- The user, not the compiler, decides whether inlining is beneficial for their use case.
  Whether it helps or hurts performance afterward is the user's concern, not the
  compiler's — the compiler's job is only to honor the mandate faithfully.

## Trade-offs

- May increase code size (duplicated function bodies at every call site).
- Purely the user's judgment call whether it's worth it for a given function.

## Implementation notes

- Inline during codegen, not in the AST/IR — keeps earlier phases simple.
- Sema needs to detect recursive `inline` calls (direct or transitive).
- Address-taking requires generating both an inline and an out-of-line version if a
  function is ever referenced by address elsewhere (though address-taking an `inline`
  function is itself an error — see above — so this may not actually be needed; worth
  revisiting once this is implemented).

## History

- Predates version tracking — carried over from the original design document, not yet
  implemented. Clarified: fully specifying the proposal was not meant to imply it was
  leaning toward acceptance — it's still completely open, deep consequences unexplored.
