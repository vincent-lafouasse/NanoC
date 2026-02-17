# NanoC Design Document

**Version:** 0.1.0
**Last Updated:** 2026-02-17

This document describes the design philosophy, architecture, and implementation details of NanoC for contributors.

## What is NanoC?

NanoC is a minimal systems programming language targeting RISC-V. It emphasizes:

- **Simplicity**: Small, predictable language with no hidden behavior
- **Transparency**: Almost 1:1 mapping to assembly
- **Explicitness**: No implicit conversions or hidden allocations
- **No Runtime**: Direct syscalls, no libc dependency

## Who This Book Is For

This book is for:
- Language designers interested in minimal language design
- Compiler contributors working on NanoC
- Systems programmers evaluating NanoC for their projects
- Anyone curious about simple, explicit programming languages

## Example

Here's a taste of NanoC:

```nanoc
struct Point {
    x: i32,
    y: i32,
}

fn distance_squared(p: Point*) -> i32 {
    return p->x * p->x + p->y * p->y;
}

fn main() -> i32 {
    var origin: Point = zeroed;
    var p: Point = Point { x: 3, y: 4, };

    const dist_sq: i32 = distance_squared(&p);

    // Syscall to write result
    syscall(1, 1, &dist_sq, 4);

    return 0;
}
```
