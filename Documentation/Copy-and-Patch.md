Copy and patch is a form of JIT originally described in [Copy-and-Patch](papers-others/Copy-and-patch-JIT.pdf). The basic idea is to stick together templates that contain the native code for various byte-codes in an interpreter.

Zag has a unique feature in that `CompiledMethod`s are not represented by a series of byte-codes, but rather a series of addresses of native functions that perform operations similar to those of byte-codes in an interpreter. They are executed in sequence via an indirect tail-call one from the other. This is not particularly space efficient (each address is 8 bytes, which kind of replace a 1 byte byte-code), but execute several times faster.

What is useful from a copy-and-patch perspective is that each of these functions is already effectively a template ready to use for a CnP JIT. The operations that link to the next threaded-function simply have to be replaced by instructions (if any) that connect to the next template. This is a natural application of Abstract Interpretation to extract the semantic part of the threaded-function from the no-longer necessary instructions that thread the words together.

This also means that by extending the Abstract Interpretation, we can evolve over time to move stack operations to register operations.

By using the threaded-functions as templates we get guaranteed correct native code generation, because the threaded functions are already working for the threaded execution. It is also trivial to extend the JIT by simply creating new threaded functions and compiling the source language to use the new operations. CnP will then seamlessly move that code into the JIT'ed code.

### The JIT'er
The `jit/cnp.zig` file has a parameterized type:
```zig
CopyAndPatch(Code, Arch, JitBuffer)
```
Where the `Code` is normally `execution.Code` the `Arch` is the particular architecture decoder/emitter and `JitBuffer` manages finding the space to allocate an executable compilation buffer.

That type includes a function
```zig
jitMethod(self: *Self, method: *const CompiledMethod) void
```
where the resulting jitted function is stored in the buffer. The result should be castable to a `*ThreadFn`.

There are 2 loops. The first goes through all the executable words, starting with the first, and called the `abstractInterpret` method to do the abstract interpretation of the machine code.

`abstractInterpret` calls the architecture type to decode instructions into an intermediate representation so we can support multiple architectures. Many of these instructions have special handling. Examples include loading things from the `pc` or the `context`, which can be recognized from the types of registers used; moving or adding to special registers, such as the `pc` or `sp`; branches.