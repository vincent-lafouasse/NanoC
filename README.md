# NanoC

A minimal systems programming language targeting RISC-V.

## Features

- **Minimal**: Small, predictable language with no hidden behavior
- **Context-free grammar**: Easy to parse and reason about
- **No runtime**: Direct syscalls, no libc dependency
- **Explicit everything**: No implicit conversions or hidden allocations
- **Readable assembly**: Almost 1:1 mapping to RISC-V instructions

## Documentation

The design documentation is available as an mdBook:

```bash
cd book
mdbook serve
```

Then open http://localhost:3000

See [`book/README.md`](book/README.md) for more details on building the documentation.

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
