# ADR-0022: Unescaped newlines in literals, and a raw string mechanism

**Status:** Open
**Area:** Language Design
**Leaning:** unescaped newlines are a hard error (settled); raw string mechanism leaning positive with OCaml-style asymmetric, quote-free delimiters, rest not yet decided

## Unescaped newlines — settled

A raw, unescaped newline byte appearing inside a string or char literal is a hard
(lex-time) error, in both directions — string literals and char literals alike. This
supersedes the lexer's actual behavior up to this point, which silently treated a literal
newline as ordinary content (an omission, not a considered decision — no `\n`-specific
case existed in the scanner at all).

Worth noting: this is one of the rare spots where NanoC keeps C's behavior rather than
diverging from it. Standard C already requires a diagnostic for a bare newline inside a
string literal (short of line-splicing via a trailing `\` before the newline) — so "hard
error" isn't a new restriction NanoC is inventing, it's just NanoC actually enforcing what
C already specifies, rather than quietly accepting more than C does.

## Raw string mechanism — open, leaning positive

Once unescaped newlines are a hard error, anyone who wants genuinely multi-line string
content — or content with enough literal backslashes that escaping every one is painful
(paths, regexes) — needs an escape hatch. Leaning positive on providing one: it doesn't
cost much (it's an alternate lexer entry point, not a change to anything already settled)
and the ergonomic win is real.

### Design questions, not yet resolved

- **Delimiter/syntax — leaning toward OCaml-style, two criteria driving it:**
  1. **Asymmetric open/close tokens**, not the same character repeated. A symmetric
     delimiter (Go's backtick, a naive quote-based raw form) makes "which occurrence
     closes the literal" purely positional — the scanner has no structural way to tell
     content from terminator besides counting. Distinct open and close tokens remove
     that ambiguity outright.
  2. **The delimiter shouldn't itself be a quote character.** A large part of the reason
     to want a raw string is to hold content containing `"` without escaping it — so a
     delimiter that's still quote-based just relocates the escaping problem rather than
     removing it. This is exactly where Rust's `r"..."` and C++'s `R"(...)"` fall short:
     both still open on `"`, so embedding a literal `"` in the content still needs the
     hash-padding escape hatch (`r#"..."#`, growing the hash count until it doesn't
     collide) — a second escaping mechanism bolted onto the first.

  Weighed against the precedents from before: Rust/C++ fail criterion 2 (still
  quote-based). Go's backtick passes criterion 2 but fails criterion 1 (symmetric).
  OCaml's `{|...|}` / `{tag|...|tag}` is the one that satisfies both at once — distinct
  open (`{|`) and close (`|}`) tokens, neither one a quote character, with the tagged
  form (`{tag|...|tag}`) only needed in the rare case where content contains `|}`
  itself. Strong leaning toward adopting this shape (or something structurally
  equivalent); exact characters not yet finalized.
- **Scope.** Presumably string literals only — a char literal is exactly one byte, so
  there's nothing for "raw" to buy you there. Should say so explicitly once decided,
  rather than leaving it implied.
- **Type and termination.** Presumably a raw string is still `ptr`-typed and
  null-terminated, consistent with
  [string-literals-and-escape-sequences](ADR-0005-string-literals-and-escape-sequences.md)
  — worth stating explicitly rather than assuming it carries over silently.
- **"Raw" isn't one toggle, it's two independent ones** — easy to conflate, worth naming
  separately:
  1. Does a literal newline byte get permitted (lifting the hard error above), or not?
  2. Does backslash keep meaning "start of an escape," or does it become an inert literal
     character?

  These vary independently, giving four combinations, not two:
  - **Neither relaxed** — today's settled default string literal (escapes active,
    newline a hard error).
  - **Newline permitted, escapes still active** — genuinely multi-line content, but
    `\n`/`\t`/etc. still decode and a literal backslash still needs `\\`. Closer to some
    languages' triple-quoted strings. Solves the multi-line motivation on its own,
    without touching backslash handling at all.
  - **Escapes inert, newline still forbidden** — a single-line "verbatim" string, useful
    specifically for backslash-heavy content (Windows paths, regexes) that doesn't need
    to span lines.
  - **Both relaxed** — the full Rust/Go-style raw string: nothing is special, a newline
    is just a byte like any other, which is *why* those designs get multi-line "for
    free" once backslash stops being an escape-introducer.

  Not yet decided whether NanoC wants one of these as a second literal form, more than
  one, or whether the "both relaxed" combination alone happens to cover every real use
  case the other two would have separately addressed.

## History

- v0.1.0: unescaped newlines settled as a hard error (both string and char literals),
  superseding the lexer's prior silent-accept behavior. Raw string mechanism proposed,
  leaning positive, design questions open.
