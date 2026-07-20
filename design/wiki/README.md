# NanoC Design Wiki

Working notes on NanoC's design: open questions, proposals, and post-MVP plans. One file
per topic, on purpose — a topic's whole history should be readable with `git log --follow`
on just that file, uncontaminated by unrelated discussion sitting in the same document.

Plain markdown, no dialect lock-in — readable anywhere (GitHub, an editor, `grep`), and
happens to also be vimwiki's markdown-syntax mode, so `[[wiki-link]]`-style navigation and
`:VimwikiFollowLink` work here without any conversion if you open it in vim with vimwiki
configured accordingly.

## Relationship to `design/book/`

`design/book/` ("A Tour of NanoC") is the settled, current-state reference — what NanoC *is*.
This wiki is where that state gets argued about before it's settled, and where decisions
that were made but don't belong in a guided tour (rejected alternatives, deferred features,
rationale) stay recorded. A topic generally starts here; once genuinely settled and load-
bearing for understanding the language as it stands, a short, decision-only summary may
also appear in the Tour, with this file linked for the full discussion.

## File shape

Filename: `ADR-NNNN-longish-greppable-name.md` — a 4-digit, zero-padded, monotonically
increasing number (never reused, never reordered — a superseded ADR keeps its number and
gets `Status: Superseded`, it doesn't free the slot) followed by a descriptive slug. The
number gives fast direct navigation (jump to `0007` in an editor); the slug keeps it
`grep`-able by keyword even without knowing the number.

Each file is small and self-contained:

```markdown
# ADR-NNNN: <Topic>

**Status:** Open | Settled | Deferred | Superseded
**Area:** Language Design | Implementation | Future Work

<Context — what's the question, why does it matter>

<Proposals / Decision — options considered, current leaning or final decision, rationale>

## History
- <version or date>: <one-line summary of what changed and why>
```

`Status` is the load-bearing field:
- **Open** — undecided, still being argued about.
- **Settled** — decided; the language behaves this way.
- **Deferred** — decided to *not* do this yet, not because it's wrong, but because
  something else has to land first, or it isn't needed until later (post-MVP).
- **Superseded** — replaced by a later decision; kept for history, cross-linked from
  whatever replaced it.

`History` is a curated, prose changelog for *this one topic* — deliberately not just a
pointer to `git log`, since a raw commit history is terse and usually touches several files
at once. Entries here should read as a short story of how the decision evolved, tagged to
the language version (`design/book/src/intro.md`'s `**Version:**`) where relevant. Where a
decision predates version tracking, say so plainly rather than inventing a date.

## Index

- [ADR-0001: Goto past a declaration](ADR-0001-backward-goto-past-declarations.md) — Mostly settled (UB, reserved for flow analysis later)
- [ADR-0002: Tuple returns and error handling](ADR-0002-tuple-returns-and-error-handling.md) — Open (no leaning)
- [ADR-0003: Assignment as expression](ADR-0003-assignment-as-expression.md) — Open (leaning: no)
- [ADR-0004: Array syntax](ADR-0004-array-syntax.md) — Open
- [ADR-0005: String literals and escape sequences](ADR-0005-string-literals-and-escape-sequences.md) — Settled (octal deferred)
- [ADR-0006: Array bounds checking](ADR-0006-array-bounds-checking.md) — Settled (UB)
- [ADR-0007: `inline` keyword](ADR-0007-inline-keyword.md) — Open (no leaning at all)
- [ADR-0008: Binding forms: `var` / `constexpr`](ADR-0008-binding-forms-var-constexpr.md) — Open (leaning toward it)
- [ADR-0009: Statement-oriented, not expression-oriented](ADR-0009-statement-oriented-expressions.md) — Settled
- [ADR-0010: Error recovery](ADR-0010-error-recovery.md) — Open (fail-fast for now, a velocity call)
- [ADR-0011: Optimization passes and debug info](ADR-0011-optimization-passes-and-debug-info.md) — Deferred (leaning: debug info yes, optimization passes maybe never)
- [ADR-0012: Compiler warnings](ADR-0012-compiler-warnings.md) — Open
- [ADR-0013: Module system and FFI](ADR-0013-module-system-and-ffi.md) — Deferred (post-MVP)
- [ADR-0014: Object file generation](ADR-0014-object-file-generation.md) — Deferred (post-MVP)
- [ADR-0015: Compile-time conditionals](ADR-0015-compile-time-conditionals.md) — Open (depends on ADR-0008)
- [ADR-0016: Stdlib strings](ADR-0016-stdlib-strings.md) — Settled (impl. deferred)
- [ADR-0017: Type system gaps: casts, pointer arithmetic, shadowing](ADR-0017-type-system-gaps.md) — Open
- [ADR-0018: Exhaustive return checking](ADR-0018-exhaustive-return-checking.md) — Deferred

## Adding a new one

Next number is `0019`. Pick a longish, greppable filename after the number — you should be
able to find a topic either by jumping straight to its number, or by `grep`-ing a keyword
(e.g. `grep -l goto design/wiki/*.md` should find the goto one by content or filename).
