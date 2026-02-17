## Implementation Details

### Memory Management

**Shared ownership:** `Rc<[u8]>` for source code and identifiers
- Zero-copy string sharing
- Reference counting for cleanup
- Immutable after creation

**AST allocation:** `Box<T>` for recursive structures
- Prevents infinite size types
- Heap allocation for tree nodes

### Performance Considerations

**Current priority:** Correctness over speed

The compiler should:
- Parse 10,000 lines in < 100ms
- Compile simple programs in < 1s
- Keep memory under 100MB for typical files

**Not priorities:**
- Parallel compilation
- Incremental compilation
- Build caching

### Error Message Quality

**Good error format:**
```
source.nc:15:8: error: type mismatch
  expected: i32
  found:    u32*
15 |     var x: i32 = ptr;
   |              ^
```

**Requirements:**
- Show file, line, column
- Display relevant source line
- Point to exact error location
- Explain what was expected vs found
- Suggest fixes where possible

---

