## Appendix

### Grammar Summary

Full grammar: `spec/grammar.ebnf`. Key productions:

```ebnf
var_decl       = "var"       variable_name ":" type "=" var_init   ";" ;
constexpr_decl = "constexpr" variable_name ":" type "=" expression ";" ;
var_init       = expression | "zeroed" | "undefined" ;
(* no runtime const — var is the only runtime binding                         *)
(* constexpr: folded, not stored, not addressable, initialiser must be a      *)
(*            constexpr expression — sema-checked, not grammar                *)

(* all var/constexpr declarations precede all statements within a block *)
block = "{" (var_decl | constexpr_decl)* statement* "}" ;

(* blocks are statement containers — no value, no trailing expression *)
block   = "{" statement* "}" ;
if_stmt = "if" "(" expression ")" block ("else" (if_stmt | block))? ;

(* bodies of if/while MUST be blocks — bare expressions are a parse error        *)
statement = const_decl
          | var_decl
          | constexpr_decl
          | lvalue "=" expression ";"    (* assignment *)
          | call_expr ";"               (* expression statement: calls/syscalls only *)
          | "return" expression? ";"
          | if_stmt
          | "while" "(" expression ")" block
          | "goto" label_name ";"
          | "unreachable" ";"
          | label_name ":" statement
          ;

(* expressions compute values — blocks and if/else are NOT expressions *)
expression = pratt_expr ;
```

### References

- RISC-V Spec: https://riscv.org/technical/specifications/
- Pratt Parsing: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
- C89 Standard (for comparison)

---

