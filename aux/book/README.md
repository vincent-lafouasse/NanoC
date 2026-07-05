# NanoC Design Book

This directory contains the NanoC design documentation as an [mdBook](https://rust-lang.github.io/mdBook/).

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
│   ├── testing-strategy.md
│   ├── contributing.md
│   ├── open-questions.md
│   ├── future-work.md
│   └── appendix.md
└── book/              # Generated HTML (after build)
```

## Editing

1. Edit markdown files in `src/`
2. Run `mdbook serve` to preview changes
3. Commit changes to version control

## Publishing

To publish online:
- GitHub Pages: `mdbook build && push to gh-pages branch`
- Any static host: Upload contents of `book/` directory
