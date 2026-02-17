## Philosophy

### Core Principles

**1. Minimal**
NanoC is intentionally small. Every language feature must justify its existence. We prefer explicit over implicit, and simple over convenient.

**2. Context-Free Grammar**
The grammar must be context-free and unambiguous. This makes parsing straightforward and reasoning about code easier.

**3. Readable Assembly**
There should be a predictable, almost 1:1 mapping from NanoC to RISC-V assembly. Programmers should be able to mentally compile their code.

**4. No Runtime**
Direct syscalls, no libc dependency. The compiler generates pure assembly with no runtime overhead.

**5. Simple Calling Convention**
Register-sized arguments only. No hidden copies, no complex ABI rules.

**6. Explicit Everything**
No implicit conversions, no type coercion, no hidden allocations. What you see is what you get.

---

