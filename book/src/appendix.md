## Appendix

### Grammar Summary

Full grammar: `spec/grammar.ebnf`. Key productions:

```ebnf
var_decl       = "var"       variable_name ":" type "=" var_init   ";" ;
const_decl     = "const"     variable_name ":" type "=" expression ";" ;
constexpr_decl = "constexpr" variable_name ":" type "=" expression ";" ;
var_init       = expression | "zeroed" | "undefined" ;
(* const:     immutable, stored, runtime — zeroed/undefined are sema errors  *)
(* constexpr: folded, not stored, not addressable — initialiser must be      *)
(*            a constexpr expression (literal / constexpr binding / comptime  *)
(*            fn call with constexpr args) — sema-checked, not grammar       *)

(* blocks and if/else are expressions; their type is their final expression's type,
 * or unit if the block ends with ";" or is empty                               *)
block   = "{" statement* expression? "}" ;
if_expr = "if" "(" expression ")" block ("else" (if_expr | block))? ;

(* bodies of if/while MUST be blocks — bare expressions are a parse error        *)
statement = const_decl
          | var_decl
          | lvalue "=" expression ";"    (* assignment: statement only *)
          | call_expr ";"               (* expression statement: calls/syscalls only *)
          | "return" expression? ";"
          | if_expr                     (* no trailing ";" needed *)
          | "while" "(" expression ")" block
          | "goto" label_name ";"
          | label_name ":" statement
          ;

expression = block | if_expr | pratt_expr ;
```

### References

- RISC-V Spec: https://riscv.org/technical/specifications/
- Pratt Parsing: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
- C89 Standard (for comparison)

---

