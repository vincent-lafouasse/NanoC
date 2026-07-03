## Philosophy

### Core Principles

**1. Minimal**
NanoC is intentionally small. Every language feature must justify its existence. We prefer explicit over implicit, and simple over convenient.

**2. C-like Syntax**
NanoC syntax is deliberately close to C — the target audience already knows C, and
familiarity lowers the learning curve. Where NanoC diverges, it does so to make parsing or
reasoning simpler: mandatory braces eliminate dangling-else ambiguity, declarations before
statements eliminate goto-past-initialisation hazards, type annotations use `:` for
consistency. The grammar is unambiguous and context-free by construction.

**3. Fundamentally Procedural and Statement-Oriented**
NanoC is a sequence of instructions, not a tree of expressions. Statements map to instructions; expressions compute values into registers. There are no expression-level side effects, no implicit control flow, no values produced by blocks or conditionals. Code should be readable as assembly — each line corresponds to something the processor does.

**4. ALU Only**
NanoC targets the integer pipeline exclusively. The FPU is a distinct hardware unit with
its own registers, instructions, and calling convention concerns — bringing it in would
widen the compiler's target without serving NanoC's purpose. NanoC is a language for bit
manipulation and symbolic computation. Programs that need real-number arithmetic use
fixed-point integers, which keeps the computation in the ALU and fully visible in the
source.

**5. No Runtime**
Direct syscalls, no libc dependency. The compiler generates pure assembly with no runtime overhead.

**6. Simple Calling Convention**
Register-sized arguments only. No hidden copies, no complex ABI rules.

**7. Explicit Everything**
No implicit conversions, no type coercion, no hidden allocations. What you see is what you get.

---

