## Experiments

### Dispatch options for interpreter
This is for the "bytecode" executer
1. Just raw send
2. Caching as per OpenSmalltalk - Monomorphic/Polymorphic/Megamorphic
3. Our dispatch, no inlining
4. Our dispatch, full inlining 1/2/3 instances of methods

### Dispatch options for compiler
Same experiments as above, but now generating native code for methods

## Development Milestones

1. Export Classes/Methods to image file - Smalltalk
    - [x] symbols
    - [ ] classes
    - [ ] methods - parse to AST
3. Load image file in interpreter - Rust
4. Pure AST interpretation
5. Compiling AST to bytecode
    - [ ] raw sends
    - [ ] parameterized inlining
6. Paper describing new dispatch in system
1. Compiling AST to bytecode
    - [ ] Deutsch cache - monomorphic
    - [ ] cache - polymorphic
    - [ ] cache - megamorphic
3. Garbage collection
1. LLVM
    - [ ] Compiling AST to LLVM-IR using raw/parameterized-inlining
    - [ ] Compiling AST to LLVM-IR using Deutsch cache
2. Plugging in type inference