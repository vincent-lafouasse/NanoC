# NanoC

A minimal systems programming language targeting RISC-V.

## Features

- **Minimal**: Small, predictable language with no hidden behavior
- **Context-free grammar**: Easy to parse and reason about
- **No runtime**: Direct syscalls, no libc dependency
- **Explicit everything**: No implicit conversions or hidden allocations
- **Readable assembly**: Almost 1:1 mapping to RISC-V instructions

## Documentation

"A Tour of NanoC" — a guided walkthrough of the language's current, settled design — is
available as an mdBook:

```bash
cd aux/book
mdbook serve
```

Then open http://localhost:3000. See [`aux/book/README.md`](aux/book/README.md) for more
details on building it.

Open questions, proposals, and post-MVP plans aren't in the Tour — see
[`aux/wiki/`](aux/wiki/), one file per topic. See [`CHANGELOG.md`](CHANGELOG.md) for what's
changed release over release.

## Quick Start

```bash
# Build the compiler
cargo build --release

# Compile a NanoC program (when implemented)
./target/release/nanoc program.nc -o program.s
```

## Project Status

Currently implementing:
- ✅ Lexer
- ✅ Parser (partial - structs, variables, expressions)
- 🚧 Semantic analysis
- 🚧 Code generation

## License

TBD
