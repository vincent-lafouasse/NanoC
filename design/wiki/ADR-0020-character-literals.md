# ADR-0020: Character literals

**Status:** Settled (syntax); type not yet stated
**Area:** Language Design

## Syntax

Single-quoted, exactly one byte:

```nanoc
'a'
'5'      // a digit char, not an int literal
' '
'\n'
'\''     // the one escape strings never need, but chars always might
'\x41'   // 'A'
```

Shares the same escape table as string literals (see
[string-literals-and-escape-sequences](ADR-0005-string-literals-and-escape-sequences.md))
— same decoding, same `\xHH`/`\dDDD`/named-escape rules — plus `\'`, which is only
necessary here since a bare `'` inside a char literal would otherwise close it early
(unlike inside a `"`-delimited string, where `'` needs no escaping at all).

## Structural errors

- `''` — empty, zero bytes. A char literal must hold exactly one.
- `'ab'` — more than one byte. Unlike C, where multi-character literals are legal
  (implementation-defined int value), NanoC rejects this outright — no legitimate reading
  of "which byte did you mean."
- `'a` (no closing quote) — unterminated, same category as an unterminated string.

## Open — what type does a char literal have?

Presumably `u8`, since it's exactly one byte — but nothing states this anywhere yet. Same
category of gap as [integer-literals](ADR-0019-integer-literals.md)'s open question about
how a `u8`-typed literal gets written; these two may end up resolved together.

## History

- v0.1.0: syntax and structural error cases proposed, matching the lexer scaffolding
  already built for them (`Token.CharLiteral`, `EmptyCharLiteral`,
  `MultiCharacterLiteral`, reusing the pre-existing `UnterminatedCharLiteral`). Literal's
  type left open.
