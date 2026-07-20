# A Tour of NanoC

This directory contains "A Tour of NanoC" — a guided walkthrough of the language's current,
settled design — as an [mdBook](https://rust-lang.github.io/mdBook/). In-progress proposals
and open questions live separately, in the design wiki at `design/wiki/`, so this tour only
ever describes what NanoC *is*, not what it might become.

## Prerequisites

Install mdBook:
```bash
cargo install mdbook
```

## Build & Serve

**Development (with live reload):**
```bash
mdbook serve
```
Then open http://localhost:3000

**Build static HTML:**
```bash
mdbook build
```
Output will be in `book/` directory.

## File Structure

```
book/
├── book.toml          # mdBook configuration
├── src/
│   ├── SUMMARY.md     # Table of contents
│   ├── intro.md       # Introduction
│   ├── philosophy.md
│   ├── language-design.md
│   ├── compiler-architecture.md
│   ├── implementation-details.md
│   ├── semantic-analysis.md
│   └── appendix.md
└── book/              # Generated HTML (after build)
```

Open questions, proposals, and post-MVP plans are not in this tour — see `design/wiki/` for
those, one file per topic.

## Editing

1. Edit markdown files in `src/`
2. Run `mdbook serve` to preview changes
3. Commit changes to version control

## Publishing

To publish online:
- GitHub Pages: `mdbook build && push to gh-pages branch`
- Any static host: Upload contents of `book/` directory
