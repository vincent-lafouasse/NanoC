## Appendix

### Grammar Summary

See `spec/grammar.ebnf` for full grammar.

Key productions:
```ebnf
program = top_level_statement* ;

top_level_statement = struct_decl
                    | var_decl
                    | function_decl
                    ;

register_sized = primitive_type | pointer ;

var_decl = ("var" | "const") variable_name ":" type "=" var_initializer ";" ;
var_initializer = expression | "zeroed" | "undefined" ;

statement = var_decl
          | expression ";"
          | "return" expression? ";"
          | "if" "(" expression ")" statement ("else" statement)?
          | "while" "(" expression ")" statement
          | "{" statement* "}"
          | "goto" label ";"
          | label ":" statement
          ;
```

### References

- RISC-V Spec: https://riscv.org/technical/specifications/
- Pratt Parsing: https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
- C89 Standard (for comparison)

---

