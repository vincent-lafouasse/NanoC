# ADR-0013: Module system and FFI

**Status:** Deferred (post-MVP)
**Area:** Future Work

**Not currently implemented.** Core language and single-file compilation comes first.

## Import mechanism

Shallow parsing on import:

```nanoc
// math.nc
pub fn sin(x: i32) -> i32 { ... }
pub fn cos(x: i32) -> i32 { ... }

// main.nc
import math;

fn main() -> i32 {
    var result: i32 = math::sin(10);
    return result;
}
```

When the compiler encounters `import math`, it performs a **shallow pass** of `math.nc`:
parse only top-level declarations (structs, function signatures, `pub` items), extract
public symbols without processing function bodies, track imported modules to detect
circular dependencies, build a symbol table for the `math::` namespace.

**Benefits:** fast compilation (no need to deeply parse the entire dependency tree), a
clear dependency graph, circular-import detection.

## Namespace and mangling

`pub` exports a symbol:

```nanoc
pub fn add(a: i32, b: i32) -> i32 { ... }  // exported
fn helper() -> i32 { ... }                  // private
```

`::` for qualified names: `math::sin(x)`, `io::write(fd, buffer)`.

Symbol mangling converts `::` to `$` for linker symbols (`std::math::sin` →
`std$math$sin`). `$` is rejected by the lexer (so users can't create conflicting symbols),
valid in most linker formats (ELF, Mach-O), and gives simple, deterministic mangling with
no hash collisions.

## Foreign Function Interface (FFI)

**Proposal:** an `extern` keyword declares unmangled symbols from linked libraries.

Basic form, no rename:

```nanoc
extern fn malloc(size: u32) -> ptr;
extern fn free(p: ptr);
extern fn write(fd: i32, buf: ptr, count: u32) -> i32;

fn main() -> i32 {
    var buffer: ptr = malloc(1024);
    free(buffer);
    return 0;
}
```

The declared name is used verbatim as the linker symbol — no NanoC mangling applied.

Rename form, importing under a different name:

```nanoc
extern "syscall" as fn libc_syscall(number: i32, ...) -> i32;
extern "write"   as fn posix_write(fd: i32, buf: ptr, n: u32) -> i32;
```

The string literal is the actual linked-object symbol name; the identifier after `as fn`
is the NanoC-side name. Needed whenever the C symbol name would collide with a NanoC
keyword — the motivating case is libc's `syscall(2)`, since NanoC already uses `syscall` as
a keyword for the raw `ecall` instruction, so a program targeting a hosted environment
that wants libc's `syscall` wrapper needs to import it under a different name:

```nanoc
extern "syscall" as fn libc_syscall(number: i32, a0: i32, a1: i32, a2: i32) -> i32;

fn main() -> i32 {
    syscall(SYS_WRITE, 1, buf, len);        // built-in: emits ecall directly
    libc_syscall(SYS_WRITE, 1, buf, len);   // goes through libc's syscall() C function
    return 0;
}
```

This is the concrete case that motivated
[making string literals always null-terminated](ADR-0005-string-literals-and-escape-sequences.md) —
an `extern`-declared C function can take a NanoC string literal with no conversion step.

**Variadic extern:** unresolved — depends on whether NanoC supports variadics at all.

**`extern` vs `foreign`:** `extern` is shorter, more familiar to C/Rust programmers.
`foreign` reads more explicitly as "this is not NanoC code." Either could work; decision
deferred until FFI is actually implemented.

`extern` declarations are never mangled, regardless of whether they appear in a `pub`
context — mangling only applies to symbols *defined* in NanoC source.

## Object file generation

See [object-file-generation](ADR-0014-object-file-generation.md).

## Module resolution

Proposed search path: current directory, `./lib/`, a system library path (e.g.
`/usr/local/nanoc/lib/`), then `NANOC_PATH`.

```nanoc
import std::math;     // looks for std/math.nc
import io::file;      // looks for io/file.nc
import mylib;         // looks for mylib.nc
```

## Open sub-questions

1. Package/crate system, or just files and directories?
2. Visibility levels — just `pub` vs. private, or more granular?
3. Re-exports — `pub import` to re-export symbols?
4. Cyclic modules — allowed if there are no circular type dependencies?

## History

- Predates version tracking — carried over from the original design document. Not
  implemented; core language and single-file compilation are prerequisite.
