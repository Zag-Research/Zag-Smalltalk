## Experiments

### Dispatch options for Zig
Generate stand-alone Zig program with the appropriate dispatch
1. Raw send
2. Deutsch cache as per OpenSmalltalk
3. Flat dispatch, no inlining
4. Our dispatch, self/super inlining
5. Our dispatch, full inlining 1/2/3 instances of methods

### Dispatch options for interpreter
This is for the "bytecode" executer
1. Just raw send
2. Caching as per OpenSmalltalk - Monomorphic/Polymorphic/Megamorphic
3. Our dispatch, no inlining
4. Our dispatch, full inlining 1/2/3 instances of methods

### Dispatch options for compiler
Same experiments as above, but now generating native code for methods

## Development Milestones

1. Generate AST forms from Opal visitor
2. Generate C stand-alone programs for benchmarking
3. Export Classes/Methods to image file - Smalltalk
    - [x] symbols
    - [ ] classes
    - [ ] methods - parse to AST
4. Load image file in interpreter - Rust
5. Pure AST interpretation
6. Compiling AST to bytecode
    - [ ] raw sends
    - [ ] parameterized inlining
7. Paper describing new dispatch in system
8. Compiling AST to bytecode
    - [ ] Deutsch cache - monomorphic
    - [ ] cache - polymorphic
    - [ ] cache - megamorphic
9. Garbage collection
10. LLVM
    - [ ] Compiling AST to LLVM-IR using raw/parameterized-inlining
    - [ ] Compiling AST to LLVM-IR using Deutsch cache
11. Plugging in type inference