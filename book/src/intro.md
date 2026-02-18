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

// fixed-point square root in Q25.6 format (6 fractional bits).
fn sqrt_q6(n: i32) -> i32 {
    const scaled: i32 = n << 12;
    if (scaled <= 0) {
        return 0;
    }

    // newton
    var x: i32 = scaled;
    var y: i32 = (scaled >> 1) + 1;
    while (y < x) {
        x = y;
        y = (y + scaled / y) >> 1;
    }

    return x;
}

// euclidean distance as Q25.6 fixed-point.
fn distance(a: Point*, b: Point*) -> i32 {
    const dx: i32 = a->x - b->x;
    const dy: i32 = a->y - b->y;
    return sqrt_q6(dx * dx + dy * dy);
}

fn main() -> i32 {
    var origin: Point = zeroed;
    var p: Point = undefined;
    p.x = 67;
    p.y = 420;

    const dist: i32 = distance(&p, &origin);  // 27219 → 425.3

    const s: ptr = "yo i'm feinberg\n\x44";

    // Syscall to write result
    const status: i32 = syscall(1, 1, &dist, 4);

    if (status < 0) {
        goto bad;
    } else {
        goto good;
    }

good:
    return 0;
bad:
    return 1;
}
```
