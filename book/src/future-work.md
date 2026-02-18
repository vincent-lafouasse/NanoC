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

**Proposal:** Use `extern` or `foreign` keyword for unmangled symbols

```nanoc
// Call C library function without mangling
extern fn malloc(size: u32) -> ptr;
extern fn free(ptr: ptr);

fn main() -> i32 {
    var buffer: ptr = malloc(1024);
    free(buffer);
    return 0;
}
```

**Alternative:** Use `foreign` to be more explicit
```nanoc
foreign fn write(fd: i32, buf: ptr, count: u32) -> i32;
```

Symbol `write` is not mangled, allowing direct calls to C functions.

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

**Document Status:** Living document, updated as design evolves.

**Next Review:** After Pratt parser implementation.
