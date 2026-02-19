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

7. **`const` semantics: runtime-immutable or compile-time constant?**

   NanoC currently uses `const` for local immutable bindings whose values are determined at runtime:

   ```nanoc
   fn sqrt_q6(n: i32) -> i32 {
       const scaled: i32 = n << 12;  // depends on argument — clearly runtime
       ...
   }
   ```

   But `const` is also the natural keyword for values the compiler must know at compile time (array sizes, `comptime if` conditions, target constants). These are fundamentally different things.

   **Option A: `const` = runtime-immutable only**
   - Current behaviour. Simple, consistent.
   - Compile-time constants need a separate mechanism (`comptime`, `static`, or built-in magic constants).
   - `var` = mutable, `const` = immutable, `comptime` = known at compile time.

   **Option B: `const` = compile-time constant only**
   - Requires a different keyword for runtime-immutable bindings (`let`, `val`, `imm`…).
   - Breaks the current feel of `const scaled: i32 = n << 12`.
   - Rust uses this split: `const` is comptime, `let` is runtime (with no mutability by default).

   **Option C: `const` is context-sensitive**
   - Compiler infers comptime vs runtime from the initialiser.
   - If the initialiser is a comptime expression → comptime constant.
   - Otherwise → runtime-immutable binding.
   - Same keyword, different semantics depending on context. Simpler syntax, harder to reason about.

   **Interaction with `comptime if`:** if `const` stays runtime-immutable, then platform-conditional constants can't use `const` directly and need either a new keyword or magic compiler-provided values (`ARCH`, `OS`, `DEBUG`).

   **Current leaning:** Option A — keep `const` as runtime-immutable, introduce explicit `comptime` only when conditional compilation is actually implemented, and rely on compiler-provided built-in constants for target discrimination in the meantime.

8. ~~**Blocks as initializer expressions, and/or `if/else` as expressions?**~~ **SETTLED**

   **Decision:** All blocks and all `if/else` are expressions unconditionally.
   Bodies of `if`, `else`, and `while` must always be blocks (mandatory braces).
   See Language Design for rationale and Grammar Summary for productions.

   Historical discussion preserved below.

   Two distinct proposals, often conflated. Worth separating.

   #### Proposal A — blocks as const/var initializers

   Allow a `{ }` block on the right-hand side of a declaration only. The block can contain
   statements and produces a value via its last expression:

   ```nanoc
   const x: i32 = {
       const allocation: MyStruct = make_a_big_object();
       const out: i32 = compute_something_else(&allocation);
       dealloc_thing(&allocation);
       out         // last expression, no semicolon → value of the block
   };
   ```

   **What this solves:**
   - `const` bindings that require multi-step initialisation or intermediate scratch values.
   - Scoped temporaries that don't leak into the outer block.
   - Cleanup before yielding a value (e.g. freeing a scratch allocation).

   **What this does not solve:**
   - Conditional values — `if/else` is still a statement so you can't branch to produce a value.
     The workaround inside the block is a mutable accumulator:
     ```nanoc
     const x: i32 = {
         var result: i32 = undefined;
         if (cond) { result = a; } else { result = b; }
         result
     };
     ```
     Not as terse as an if-expression, but the intent is explicit.

   **The implicit semicolon rule is contained:**
   The "last expression without a semicolon is the value" rule only applies in one syntactic
   position — the right-hand side of a declaration. It cannot be triggered accidentally inside
   an arbitrary expression context.

   **Alternative — explicit `yield` instead of implicit last-expression:**
   ```nanoc
   const x: i32 = {
       const allocation: MyStruct = make_a_big_object();
       const out: i32 = compute_something_else(&allocation);
       dealloc_thing(&allocation);
       yield out;    // explicit — unambiguous, greppable
   };
   ```
   More verbose for simple cases but removes the semicolon footgun entirely.

   **Checking that `const` is written once:** verifying single-assignment for a `const` that is
   conditionally assigned across branches is complex (requires dataflow). Initializer blocks
   sidestep this entirely — a `const` either has an initializer expression (including a block)
   or it doesn't. No post-declaration assignment is ever valid.

   #### Proposal B — `if/else` as an expression everywhere

   Allow `if (cond) { ... } else { ... }` to appear anywhere a value is expected:

   ```nanoc
   const x: i32 = if (cond) { a } else { b };
   fn_call(if (flag) { x } else { y });
   ```

   **What this adds over Proposal A:**
   - Conditional values without a mutable accumulator.
   - Eliminates the need for a ternary `?:` operator.
   - Makes `comptime if` work as an expression (unblocks the Future Work constraint).

   **Additional cost over Proposal A:**
   - `if` now has two roles (statement and expression); context determines which.
   - `return` inside an if-expression branch unambiguously returns from the *function*, not the
     if — correct but surprising:
     ```nanoc
     const x: i32 = if (cond) { return -1; } else { 42 };
     //                         ^^^^^^^^^^
     //                         exits the function, not just the if-branch
     ```

   #### Current leaning — uniform expressions everywhere

   Make all blocks and all `if/else` expressions unconditionally, not just in initializer
   position. Reasoning:

   - The rule is uniform and simple to specify: every block produces a value (or unit).
   - `const` must always have an initializer — `const x: i32 = undefined` becomes a parse
     error, which is correct. Only `var` may use `= undefined` or `= zeroed`.
   - No special initializer-only position to track in the grammar or semantic analysis.
   - Semantic analysis for expression typing is more work than the current nothing, but less
     work than dataflow analysis for "was this `const` assigned exactly once across branches".

   **Type system implications this requires:**

   `if` without `else` in expression position: a bare `if (cond) { expr }` has type `unit`
   and may only appear in statement position (value discarded). The type checker enforces this.
   `if/else` with matching branch types produces that type as a value.

   A **never/bottom type** for diverging branches — `return`, `goto`, `unreachable` inside an
   expression branch never produce a value and must unify with any type:
   ```nanoc
   const x: i32 = if (cond) { 42 } else { return -1; };
   //                                      ^^^^^^^^^^
   //                                      type: never — unifies with i32, accepted
   ```
   The never type need not be user-facing syntax; it is an internal type checker concept.

   **The semicolon rule becomes universal:** `{ stmts; expr }` produces `expr` as the block
   value; `{ stmts; expr; }` produces unit. Applies everywhere, not just in special positions.
   More consistent, though the implicit nature of the rule remains.

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

