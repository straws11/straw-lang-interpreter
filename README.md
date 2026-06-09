# straw-lang-interpreter
Interpreter for StrawLang written in OCaml

# To-Do Features

- [x] Arrays (2d included)
- [x] Formatted strings natively
    - Currently supports only variables of type string, no expressions or implicit type conversion
- [x] Stdlib for print, read input
- [x] Structs
- [x] Enum
- [ ] match statement
- [ ] allow single statement blocks for for/while/if
- [x] Else-if support
- [x] Allow closures
- [x] stdlib for type conversion
- [ ] Closures to only capture relevant information about environment
- [ ] More array operations, currently messy
- [x] Allow struct field modification (cannot assign yet)
- [x] Allow array value modification (cannot assign yet)
- [ ] Better array initialization like let arr = int[10]

# Planned Order
1. multi-file / imports
2. stdlib written in StrawLang
3. match statement
4. grammar.js / tooling polish
5. syntactic sugar (single-line bodies)
