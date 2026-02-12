# NanoC

A minimal systems programming language that compiles to RISC-V assembly.

## Design Goals

- **Context-free grammar** - easy to parse and reason about
- **Readable assembly** - predictable mapping to RISC-V instructions
- **Minimal type system** - primitives and typed pointers, no implicit conversions
- **No runtime** - direct syscalls, no libc dependency
- **Simple calling convention** - register-sized arguments only

## Language Features

- Primitive types: `u8`, `i32`, `u32`, `ptr`
- Typed pointers: `Point*`, `Buffer*` (type-safe struct access)
- Static arrays: `i32[10]`, `u8[256]`
- Structs (pass by pointer)
- Direct syscalls via `syscall` keyword
- Control flow: `if`/`else`, `while`, `break`, `continue`, `goto`
- Operators: arithmetic, bitwise, comparison, logical
- Immutable bindings with `const`

## Example

```c
struct Point {
    x: i32;
    y: i32;
}

fn point_add(a: Point*, b: Point*, result: Point*) {
    result->x = a->x + b->x;
    result->y = a->y + b->y;
}

fn main() -> i32 {
    const msg: ptr = "Hello, NanoC!\n";
    syscall(SYS_write, 1, msg, 14);
    return 0;
}
```

## Building

```bash
cargo build --release
```

## Usage

Lex a file:
```bash
cargo run -- test.nc
```

Lex a code string:
```bash
cargo run -- -c "fn main() -> i32 { return 0; }"
```

## Project Structure

```
spec/grammar.y          # Yacc grammar definition
src/lexer.rs           # Lexer implementation
examples/              # Language examples
  01_basics.nc         # Variables, arithmetic, functions
  02_loops.nc          # Control flow
  03_pointers.nc       # Pointer operations
  04_structs.nc        # Struct definitions
  05_bitwise.nc        # Bit manipulation
  06_arrays.nc         # Array operations
  07_syscalls.nc       # System calls
  08_typed_pointers.nc # Type-safe pointers
syntax/nanoc.vim       # Vim syntax highlighting
```

## Documentation

- `docs/TYPE_SYSTEM.md` - Type rules and pointer semantics
- `CLI_USAGE.md` - Command-line interface guide

## Current Status

- [x] Grammar defined
- [x] Token definitions
- [x] CLI driver
- [ ] Lexer implementation
- [ ] Parser
- [ ] Type checker
- [ ] RISC-V codegen

## License

MIT
