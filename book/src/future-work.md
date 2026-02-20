## Future Work

### Module System (Post-MVP)

**Not currently implemented.** Design proposal for when modularization becomes necessary:

#### Import Mechanism

**Shallow parsing on import:**
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
- Parse only top-level declarations (structs, function signatures, pub items)
- Extract public symbols without processing function bodies
- Track imported modules to detect circular dependencies
- Build a symbol table for `math::` namespace

**Benefits:**
- Fast compilation (don't parse entire dependency tree deeply)
- Clear dependency graph
- Circular import detection

#### Namespace and Mangling

**Public symbols:** Use `pub` keyword to export
```nanoc
pub fn add(a: i32, b: i32) -> i32 { ... }  // Exported
fn helper() -> i32 { ... }                  // Private
```

**Namespace access:** Use `::` for qualified names
```nanoc
math::sin(x)
io::write(fd, buffer)
```

**Symbol mangling:** Convert `::` to `$` for linker symbols
```
Source:  std::math::sin
Symbol:  std$math$sin
```

**Rationale:** `$` is:
- Rejected by the lexer (so users can't create conflicting symbols)
- Valid in most linker formats (ELF, Mach-O)
- Simple deterministic mangling (no hash collisions)

#### Foreign Function Interface (FFI)

**Proposal:** Use `extern` keyword to declare unmangled symbols from linked libraries.

**Basic form — no rename:**
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

**Rename form — import under a different name:**
```nanoc
extern "syscall" as fn libc_syscall(number: i32, ...) -> i32;
extern "write"   as fn posix_write(fd: i32, buf: ptr, n: u32) -> i32;
```

The string literal is the actual symbol name in the linked object; the identifier after `as fn` is the NanoC-side name. This is necessary whenever the C symbol name would collide with a NanoC keyword or built-in. The motivating case is libc's `syscall(2)`: NanoC already uses `syscall` as a keyword for the raw `ecall` instruction, so any program that wants to call libc's `syscall` wrapper (e.g. when targeting a hosted environment rather than bare metal) needs to import it under a different name:

```nanoc
extern "syscall" as fn libc_syscall(number: i32, a0: i32, a1: i32, a2: i32) -> i32;

fn main() -> i32 {
    // NanoC built-in: emits ecall directly
    syscall(SYS_WRITE, 1, buf, len);

    // libc wrapper: goes through libc's syscall() C function
    libc_syscall(SYS_WRITE, 1, buf, len);

    return 0;
}
```

**Variadic extern:** Unsure how to interface with variadic C since it's unsure if we'll support variadics at all

**Design note — `extern` vs `foreign`:** `extern` is shorter and more familiar to C/Rust programmers. `foreign` reads more explicitly as "this is not NanoC code". Either could work; decision deferred until FFI is implemented.

**Symbol mangling interaction:** `extern` declarations are never mangled regardless of whether they appear in a `pub` context. Mangling only applies to symbols *defined* in NanoC source.

#### Object File Generation

**Current:** Compiler emits `.s` assembly text
```
nanoc file.nc -> file.s -> (as) -> file.o -> (ld) -> binary
```

**Future:** Emit relocatable object files directly
```
nanoc file.nc -> file.o -> (ld) -> binary
```

**Benefits:**
- Faster compilation (skip assembler invocation)
- Better integration with existing build systems
- Can generate debug info (DWARF) directly
- Enables true separate compilation

**Implementation notes:**
- Use a library like `object` crate or write ELF/Mach-O directly
- Generate relocation entries for cross-module references
- Emit symbol tables with mangled names
- Support incremental compilation (only recompile changed modules)

#### Module Resolution

**Proposed search path:**
1. Current directory
2. `./lib/` subdirectory
3. System library path (e.g., `/usr/local/nanoc/lib/`)
4. `NANOC_PATH` environment variable

**Module naming:**
```nanoc
import std::math;     // Looks for std/math.nc
import io::file;      // Looks for io/file.nc
import mylib;         // Looks for mylib.nc
```

#### Open Design Questions

1. **Package/crate system?** Or just files and directories?
2. **Visibility levels?** Just `pub` vs private, or more granular?
3. **Re-exports?** `pub import` to re-export symbols?
4. **Cyclic modules?** Allow if no circular type dependencies?

**Current status:** Not implemented. Core language and single-file compilation comes first.

---

### Compile-Time Conditionals (Post-MVP)

**Not currently implemented.** Needed for portable code that targets multiple platforms (RISC-V vs ARM, bare-metal vs Linux, debug vs release).

#### The need

Without some form of compile-time branching, any program that must behave differently per target is forced to either maintain separate source files or hardcode one platform. The canonical example is platform-specific intrinsics:

```nanoc
// want this on riscv64:
fn flush_icache() { fence_i(); }

// want this on aarch64:
fn flush_icache() { isb(); }
```

#### Design

Three keywords, resolved (see Open Questions §7):

```nanoc
var   x: i32 = expr;      // mutable, stored
const x: i32 = expr;      // immutable, stored, runtime value — NOT folded
constexpr x: i32 = expr;  // immutable, folded, compile-time only — not addressable
```

**`comptime fn`** — marks a function as evaluable at compile time. Must satisfy additional
sema restrictions (see below). Called normally at runtime; evaluated by the compiler when
appearing in a `constexpr` initialiser or `comptime if` condition:

```nanoc
comptime fn page_count(size: i32, page: i32) -> i32 {
    return (size + page - 1) / page;
}

constexpr PAGES: i32 = page_count(65536, 4096);  // evaluated at compile time → 16
var buf: ptr = malloc(page_count(n, 4096));        // called normally at runtime
```

`comptime` and `inline` are orthogonal: `comptime inline fn` is evaluated at compile time
in constexpr/comptime-if contexts and always inlined at runtime call sites.

**`comptime fn` sema restrictions:**
- May only call other `comptime` functions — no runtime-only calls, no `syscall`
- Parameters must be `constexpr`-typed when called from a comptime context
- No `undefined` or `zeroed` local initialisers — every value must be statically known
- No `while` loops initially — termination cannot be proved; relax later if needed
- No global mutable state access

**`constexpr` initialiser rule:** must be a *constexpr expression* — a literal, a
`constexpr` binding, or a call to a `comptime fn` with `constexpr` arguments. Sema error
otherwise. `&constexpr_binding` is always a sema error (no storage exists to point at).

**Compiler-provided target constants** — always `constexpr`, defined by the compiler:

```
ARCH    "riscv64" | "aarch64"
OS      "linux" | "bare"
OPT     "debug" | "release"
```

**`comptime if`** — `comptime` keyword preceding an `if_expr`. The condition must be a
constexpr expression. Dead branches are still parsed and type-checked (unlike the C
preprocessor). May appear at statement level or in expression position:

```nanoc
// statement level — selects between function definitions
comptime if (ARCH == "riscv64") {
    fn flush_icache() { fence_i(); }
} else {
    fn flush_icache() { isb(); }
}

// expression level — selects a compile-time value
constexpr PAGE_SIZE: i32 = comptime if (ARCH == "riscv64") { 4096 } else { 65536 };
```

`comptime` does not apply to arbitrary expressions (unlike Zig). Compile-time evaluation
only occurs in `constexpr` initialisers and `comptime if` conditions.

#### Open sub-questions

- Do symbols declared inside a `comptime if` block (statement level) escape into the
  enclosing scope?
- Can `comptime if` appear inside a function body, or only at top level?
- Can `comptime fn` contain `if/else` expressions? (Yes, probably — they are pure.)
- How does this interact with the module system?

**Current status:** Not implemented. Design settled; implementation deferred post-MVP.
