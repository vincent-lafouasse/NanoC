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
