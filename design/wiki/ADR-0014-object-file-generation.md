# ADR-0014: Object file generation

**Status:** Deferred (post-MVP)
**Area:** Future Work

## Current

The compiler emits `.s` assembly text:

```
nanoc file.nc -> file.s -> (as) -> file.o -> (ld) -> binary
```

## Future

Emit relocatable object files directly:

```
nanoc file.nc -> file.o -> (ld) -> binary
```

**Benefits:** faster compilation (skip the assembler invocation), better integration with
existing build systems, direct debug-info (DWARF) generation, true separate compilation.

## Implementation notes

- Use a library (e.g. Rust's `object` crate equivalent, if one exists for the OCaml
  toolchain) or write ELF/Mach-O directly.
- Generate relocation entries for cross-module references.
- Emit symbol tables with mangled names — depends on
  [module-system-and-ffi](ADR-0013-module-system-and-ffi.md)'s mangling scheme.
- Support incremental compilation (only recompile changed modules).

## History

- Predates version tracking — carried over from the original design document. Not
  started; the compiler currently only targets emitting `.s` text.
