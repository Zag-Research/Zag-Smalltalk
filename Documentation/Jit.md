## The JIT

Just In Time compilation means that the code is generated during program execution. Sometimes this is before a given method is run the first time; sometimes the method runs a few times to gather information about the execution.

We choose for the default to be machine code. So if a method is to be interpreted, it will have a tiny machine code header that will verify the appropriate method is called, and then jump to the interpreter to interpret the AST.

The JIT uses [Interpreter](Interpreter.md#Method%20dispatch)

### LLVM
 - 
### Eclipse-OMR
 - [Eclipse-OMR](https://eclipse-omr.org)
### OpenJIT
- [openjit.org](https://www.openjit.org/)