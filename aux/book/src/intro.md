# A Tour of NanoC

**Version:** 0.1.0
**Last Updated:** 2026-07-20

This is a guided walkthrough of NanoC's current design — the settled language, architecture,
and implementation, as it stands today. It describes what NanoC *is*, not what it might
become: in-progress proposals, open questions, and post-MVP plans live in the design wiki
(`aux/wiki/`) instead, so this tour stays a reliable, coherent read start to finish rather
than a mix of decided and undecided material.

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
    var scaled: i32 = n << 12;
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
    var dx: i32 = a->x - b->x;
    var dy: i32 = a->y - b->y;
    return sqrt_q6(dx * dx + dy * dy);
}

fn main() -> i32 {
    var origin: Point = zeroed;
    var p: Point = undefined;
    p.x = 67;
    p.y = 420;

    var dist: i32 = distance(&p, &origin);  // 27219 → 425.3

    var s: ptr = "yo i'm feinberg\n\x44";

    constexpr SYS_WRITE: i32 = 67;
    constexpr STDOUT: i32 = 1;
    var status: i32 = syscall(SYS_WRITE, STDOUT, s, 16);

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
