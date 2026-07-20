# ADR-0001: Goto past a declaration

**Status:** Mostly settled — leaning UB, with a reserved option to revisit via flow
analysis, depending on how far sema ends up going (not yet known)
**Area:** Language Design

## Context

Two related hazards, both about a `goto` and a declaration crossing paths:

**Backward** — a jump lands *before* a declaration and re-executes it:

```nanoc
here:
    var x: i32 = 3;  // What happens on second iteration?
    goto here;
```

**Forward** — a jump skips *past* a declaration into code that assumes it already ran:

```nanoc
fn f() -> i32 {
    while (x > 0) {
        while (y > 0) {
            if (done) goto cleanup;
        }
    }
    var result: i32 = compute();  // skipped if `done` was true
cleanup:
    return result;                // uses result — was it initialized?
}
```

The forward case is the more important one in practice — it's the shape of the canonical
`goto cleanup` pattern the Tour's Language Design chapter already shows as the reason
`goto` replaced `break`/`continue`, so whatever the answer is, it has to keep that pattern
usable.

## Options considered

1. **Allow re-execution, define forward-skip as an error** — match C's backward behavior,
   statically forbid the forward case.
2. **Static error for both directions** — forbid any jump whose path crosses a declaration.
   Requires real reachability analysis: for a `goto`/label pair, does *some* path from the
   goto to the label pass through the declaration without also passing through a point
   where it's already been evaluated? That's meaningfully more than a same-block textual
   check — a same-block-only version doesn't catch the `goto cleanup` example above, since
   the `goto` and the declaration it skips aren't in the same block.
3. **Structural prevention** — require all declarations to precede all statements
   (including labels) in every block, making the hazard unwritable rather than caught.
   Turns out to conflict with real code: the Tour's own `sqrt_q6` example interleaves
   declarations and statements in exactly the way this would forbid.
4. **UB** — don't check either direction. Document it, same as division by zero, null
   deref, and array bounds (see [array-bounds-checking](ADR-0006-array-bounds-checking.md)).

## Decision

Leaning strongly toward option 4 — UB, both directions. Consistent with the reasoning
already settled for array bounds: a real static check here isn't a cheap structural rule
(option 3 doesn't actually work, as the `sqrt_q6` counterexample shows) — it's proper flow
analysis, and paying for that isn't obviously worth it when the failure mode is squarely
the programmer's own responsibility to avoid. If you write `goto cleanup` past a
declaration you meant to run first, that's your gun, your foot, same as any other UB case
already in the language.

**The reservation:** this is *mostly* settled, not fully closed, because the actual cost
of option 2 (proper flow analysis) isn't known yet — the extent of sema as a whole is
still undetermined. If sema ends up needing CFG-shaped analysis for other reasons anyway
(see [exhaustive-return-checking](ADR-0018-exhaustive-return-checking.md), which has the
same "non-trivial in the presence of `goto`" shape), catching this specific hazard might
become cheap enough, incidentally, to be worth doing properly instead. Revisit then, not
before.

## Open follow-ons this creates (not yet resolved)

Accepting UB here doesn't automatically answer these — they're real, immediate
consequences that still need their own decision:

- **Does "declarations precede all statements in a block" still hold as a blanket rule?**
  If skipping a declaration is just UB, the rule's entire reason to exist — making the
  skip unwritable — is gone. It could be relaxed to match how code like `sqrt_q6` is
  already written (interleaved), rather than requiring `sqrt_q6` to be rewritten to
  comply with a rule that no longer needs to be that strict.
- **Does "goto into a nested block is an error" (currently a hard sema error, not UB)
  still make sense as a hard error?** Its stated rationale is entirely built on the
  strict declarations-before-statements rule holding. If that rule is relaxed, jumping
  into a nested block that skips a declaration there is just the same UB as every other
  case here — the hard error might no longer be pulling its weight either, or might need
  a narrower, different justification to survive.

Both are live questions, deliberately left open here rather than decided as a side effect.

## A cost worth naming, not glossing over

`var`'s mandatory-initialization rule and the `undefined` keyword exist specifically so
uninitialized state is never *accidental* — you must write `undefined` to opt into it.
A `goto` skipping a real initializer (`var result: i32 = compute();` never runs) produces
the identical indeterminate-value hazard without anyone writing `undefined` anywhere — a
second, silent path to exactly what that keyword was built to make loud. Accepted as a
trade-off here, not an oversight.

## History

- Predates version tracking — carried over from the original design document as the
  backward-only question, originally leaning toward a dedicated static error.
- Corrected twice in the same discussion: first toward "structural prevention via
  declarations-before-labels," then found to conflict with the Tour's own `sqrt_q6`
  example and to miss the forward-skip `goto cleanup` case entirely; settled on UB for
  both directions instead, with flow analysis kept as an explicit option to revisit later
  depending on how sema's scope grows.
