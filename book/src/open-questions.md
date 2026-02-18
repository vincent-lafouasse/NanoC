## Open Questions

### Language Design

1. **Backward goto past declarations:** Allow or forbid?
   - C allows (re-initializes)
   - Could be confusing
   - **Proposal:** Static error

2. **Tuple returns and error handling:** Multiple proposals for using both RISC-V return registers (a0, a1)

   **Background:** RISC-V calling convention provides two return registers (a0 and a1). Most functions only use a0. We could use both registers for richer return semantics.

   **Proposal A: Simple tuples (minimal)**
   ```nanoc
   fn divmod(a: i32, b: i32) -> (i32, i32) {
       return (a / b, a % b);
   }

   var quot: i32;
   var rem: i32;
   (quot, rem) = divmod(10, 3);
   ```
   - Pro: Minimal syntax, explicit
   - Con: Doesn't address error handling ergonomics

   **Proposal B: Tuple-based Result pattern (convention)**
   ```nanoc
   // Convention: (value, error_code)
   // a0 = value (or garbage if error)
   // a1 = error code (0 = success)

   fn divide(a: i32, b: i32) -> (i32, i32) {
       if (b == 0) {
           return (0, 1);  // ERR_DIV_ZERO
       }
       return (a / b, 0);  // value, no error
   }

   var result: i32;
   var err: i32;
   (result, err) = divide(10, 0);
   if (err != 0) {
       // Handle error
   }
   ```
   - Pro: Uses both registers, explicit error handling
   - Con: Very verbose for error propagation

   **Proposal C: Zig-style `try` (ergonomic)**
   ```nanoc
   fn process_file() -> (i32, i32) {
       const fd = try open_file("data.txt");  // Auto-propagate on error
       const data = try read_data(fd);
       const result = try process(data);
       return (result, 0);
   }

   // `try` desugars to:
   // var fd: i32;
   // var err: i32;
   // (fd, err) = open_file("data.txt");
   // if (err != 0) { return (undefined, err); }
   ```
   - Pro: Ergonomic, explicit keyword, makes error handling practical
   - Con: Adds keyword, implicit control flow (early return)
   - Similar to Zig's error handling (proven in practice)

   **Current leaning:**
   - Proposal A (simple tuples) for general use
   - Either skip Result pattern entirely (too verbose without `try`)
   - OR add `try` keyword (Proposal C) if error handling becomes important

   **Key insight:** Without `try` semantics, the Result pattern is too verbose to be practical. Either commit to `try` or stick with traditional C error codes.

3. **Assignment as expression:** Should `x = value` be an expression or a statement?

   In C, assignment is an expression that returns the assigned value, enabling chained assignment (`a = b = 0`) and assignment in conditions (`while ((n = read()) > 0)`).

   **Option A: Statement only (current leaning)**
   ```nanoc
   x = value;          // ✅ valid as a statement
   var y = x = 0;      // ❌ compile error
   if (x = read()) {}  // ❌ compile error (also: warning d) would catch this)
   ```
   - Eliminates the classic `if (x = 0)` bug class entirely
   - Simpler grammar: assignment is its own statement form, not an operator
   - The `Precedence::Assignment` slot in the Pratt table is still useful as a floor when parsing the RHS of assignment statements

   **Option B: Expression (C-style)**
   ```nanoc
   a = b = 0;           // ✅ right-associative, both set to 0
   while ((n = read()) > 0) {}  // ✅ assign and test in one
   ```
   - More expressive, familiar to C programmers
   - `a = b = 0` maps naturally to the Pratt parser (right-associative, same precedence for recursive call)
   - Risk: assignment in condition is a common source of bugs

   **Current leaning:** Statement only. The bug prevention outweighs the expressiveness, and NanoC already uses `goto` for the patterns that would otherwise motivate assignment-as-expression.

4. **Array syntax:** Static arrays in grammar but not implemented
   - Should be `i32[10]` or `[10]i32`?
   - Stack allocated only?
   - **Proposal:** `i32[10]` (postfix, matches pointers)

4. **String literals:** Currently `ptr` type
   - Should be `u8*` for safety?
   - Null-terminated?
   - **Proposal:** Unsure if we want null termination

5. **Array bounds checking:** Leave as UB or define behavior?
   - UB (performance, matches C)
   - Trap/panic (safety)
   - Return zero (defined but misleading)
   - **Current:** UB (same as C), may add optional checking

6. **`inline` keyword:** Force function inlining at call sites

   **Proposal:**
   ```nanoc
   inline fn square(x: i32) -> i32 {
       return x * x;
   }

   fn compute() -> i32 {
       return square(5);  // Always inlined
   }
   ```

   **Semantics:**
   - The `inline` keyword guarantees the function will be inlined at every call site
   - This is NOT a hint or optimization suggestion - it is a mandate
   - The compiler must inline the function or fail with an error if inlining is impossible
   - Recursive calls to `inline` functions are a compile error
   - Taking the address of an `inline` function is a compile error

   **Rationale:**
   - Provides deterministic performance characteristics (no call overhead)
   - Makes code generation predictable for users who need it
   - Users, not the compiler, decide if inlining is beneficial for their use case
   - Useful for hot paths where call overhead matters
   - Explicit control over the generated assembly (fits NanoC's philosophy)

   **Trade-offs:**
   - May increase code size (duplicated function bodies)
   - User is responsible for deciding if inlining is actually beneficial
   - Whether it improves or harms performance is the user's concern, not the compiler's

   **Implementation notes:**
   - Inline during codegen phase, not in AST/IR
   - May need to detect recursive inline calls during semantic analysis
   - Function address-taking requires generating both inline and out-of-line versions

   **Status:** Proposed, not yet implemented

### Implementation

1. **Error recovery:** Should parser attempt to continue after errors?
   - Pro: Report multiple errors at once
   - Con: More complex parser
   - **Current:** Fail fast (report first error only)

2. **Optimization passes:** Add any?
   - Dead code elimination?
   - Constant folding?
   - **Current:** None (keep codegen simple)

3. **Debug info:** Generate DWARF?
   - Useful for debugging
   - Adds complexity
   - **Proposal:** Later, not MVP

4. **Compiler warnings:** Which warnings should the compiler emit?

   **Rationale:** While NanoC aims for simplicity, some code patterns are error-prone or surprising. Warnings can help users catch mistakes without adding language complexity.

   **Proposed warnings:**

   **a) Precedence warnings** (Priority: HIGH)

   Warn about potentially confusing operator precedence, especially where bitwise operators mix with comparison/equality operators.

   ```nanoc
   // Warning: '&' has lower precedence than '=='; '==' will be evaluated first
   if (x & 0xFF == 0) { ... }
   //    ^~~~~~~~~~~
   // Suggestion: Use parentheses: (x & 0xFF) == 0
   ```

   This is surprising because:
   - Most programmers expect bitwise AND to bind tighter than equality
   - C's precedence puts equality above bitwise operators (inherited from B language)
   - Even experienced programmers get this wrong

   Similar warnings for:
   - `x | FLAG == y` → suggest `(x | FLAG) == y`
   - `x ^ y == 0` → suggest `(x ^ y) == 0`
   - `a < b & mask` → suggest `a < (b & mask)` or `(a < b) & mask`

   **b) Unused variable warnings**
   ```nanoc
   var temp: i32 = compute();  // Warning: variable 'temp' is unused
   ```

   **c) Const variable never read**
   ```nanoc
   const MAX: i32 = 100;  // Warning: constant 'MAX' is never used
   ```

   **d) Suspicious assignment in condition**
   ```nanoc
   if (x = 5) { ... }  // Warning: using assignment in condition (did you mean '=='?)
   ```
   Note: NanoC doesn't support this syntax in conditions, but if we add it, warn about it.

   **e) Unreachable code**
   ```nanoc
   fn example() -> i32 {
       return 42;
       var x: i32 = 0;  // Warning: unreachable code
   }
   ```

   **f) Function declared but never called** (for non-exported functions)
   ```nanoc
   fn helper() -> i32 {  // Warning: function 'helper' is never used
       return 5;
   }
   ```

   **g) Division by literal zero** (Priority: HIGH, candidate for hard error)

   Division by zero is UB in NanoC. When the divisor is a literal `0`, the compiler can catch it statically.

   ```nanoc
   var x: i32 = a / 0;   // Error: division by zero
   var y: i32 = a % 0;   // Error: division by zero
   ```

   **Proposal:** Promote this to a hard compile error rather than a warning, since there is no legitimate use case for dividing by a literal zero. A warning would suggest the code might be acceptable — it never is.

   Note: This only applies to literal `0`. Division by a variable whose runtime value happens to be zero remains UB and is the programmer's responsibility to guard against.

   **Implementation approach:**
   - Warnings are purely optional and don't affect compilation
   - Can be enabled/disabled via compiler flags (e.g., `-Wall`, `-Wno-precedence`)
   - Should not impact the simplicity of the core compiler
   - Could be implemented as a separate analysis pass after parsing

   **Status:** Proposed
   - Precedence warnings: HIGH priority (genuinely confusing)
   - Other warnings: MEDIUM priority (nice to have)

---

