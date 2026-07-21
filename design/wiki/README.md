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
**Leaning:** <short free-text summary, optional>

<Context — what's the question, why does it matter>

<Proposals / Decision — options considered, current leaning or final decision, rationale>

## History
- <version or date>: <one-line summary of what changed and why>
```

The header is machine-checked, by `make_index.py` (see below) — it hard-errors on any ADR
that doesn't match this exactly, so the format isn't just a convention, it's enforced.
Concretely: `Status` and `Area` must each be *exactly* one of their listed values, one
value, nothing appended — no parentheticals, no dashes, no "(see below)". Anything more
nuanced than the bare category goes in `Leaning` or in the body, never in `Status`/`Area`
themselves.

`Status` is the load-bearing field:
- **Open** — undecided, still being argued about.
- **Settled** — decided; the language behaves this way.
- **Deferred** — decided to *not* do this yet, not because it's wrong, but because
  something else has to land first, or it isn't needed until later (post-MVP).
- **Superseded** — replaced by a later decision; kept for history, cross-linked from
  whatever replaced it.

`Leaning` is where the nuance that used to get crammed into `Status` lives instead — a
short phrase like "UB" or "toward it, not fully sold" or "depends on ADR-0008". Optional:
omit it entirely when `Status` alone already says everything (a cleanly `Settled` ADR with
no remaining wrinkle doesn't need one). It shows up next to `Status` in the generated
index, so it's the thing a reader sees before ever opening the file.

`History` is a curated, prose changelog for *this one topic* — deliberately not just a
pointer to `git log`, since a raw commit history is terse and usually touches several files
at once. Entries here should read as a short story of how the decision evolved, tagged to
the language version (`design/book/src/intro.md`'s `**Version:**`) where relevant. Where a
decision predates version tracking, say so plainly rather than inventing a date.

## Index

Generated, not hand-maintained — see [`index.md`](index.md). Regenerate it with:

```bash
python3 make_index.py
```

after adding, renaming, or changing the status of any ADR. It rebuilds three views —
chronological (by number), grouped by status, grouped by area — and hard-errors instead of
silently producing a stale or wrong index if any file's header doesn't parse.

## Adding a new one

Next number is `0022`. Pick a longish, greppable filename after the number — you should be
able to find a topic either by jumping straight to its number, or by `grep`-ing a keyword
(e.g. `grep -l goto design/wiki/*.md` should find the goto one by content or filename).
Run `make_index.py` afterward to refresh `index.md`.
