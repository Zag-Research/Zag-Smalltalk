Primitives are how a Smalltalk system connects to the underlying hardware, including basic operations. They are declared as a pragma at the start of a method. Zag is no different.

An example (`SmallInteger>>#+`) is:
```smalltalk
+ aNumber
	"Primitive. Add the receiver to the argument and answer with the result
	if it is a SmallInteger. Fail if the argument or the result is not a
	SmallInteger  Essential  No Lookup."

	<primitive: 1>
	^ super + aNumber
```
The semantics is that when this method is invoked, the `primitive 1` code is executed, without even creating any context. If the primitive succeeds, the send is complete and the original sender is resumed with the parameters having been removed, and the result pushed on the stack.

If the primitive fails, the parameters are left on the stack, and the Smalltalk code is executed exactly the same as if there had been no primitive pragma in the first place. The primitive can fail for many reasons, for example in the case of `SmallInteger>>#+` the parameter might not be a small integer, or the result might be bigger than can be represented. The fall-through allows decisions to be made about how to handle the failure.

There are 4 versions of the primitive pragma:
1. `<primitive: v>`
2. `<primitive: v error: e>`
3. `<primitive: v module: m>`
4. `<primitive: v module: m error: e>`

For the `error:` cases, the primitive leaves a value on the stack, and the compiler treats the named variable (`e`) as an additional parameter. For the `module:` cases, `v` and `m` are both strings identifying a particular named primitive within a particular named module. In the other cases, the primitive (`v`) is normally a small integer. There are a couple of cases in the Pharo image where there are string argument to the first 2 cases. They will be translated by the compiler to the module forms with a module of `'default'`.

Primitive methods in Zag are also treated somewhat specially. The first code word of the method and the initial value of the `executeFn` field in the method will be one of four primitive code words: `primitive`, `primitiveError`, `primitiveModule`, or `primitiveModuleError`. They all perform essentially the same operations, differing only in how many objects follow them in the code array, and how they look up the correct primitive. The sequence is:
1. check the `extra` parameter to see if it is encoded or not - on the first call it will not be;
2. if it is not encoded, locate the primitive code, update `executeFn` and `jittedFn` to reference it, and then pass control to that code;
3. if it is encoded, then we have just been called from the primitive code because there was a primitive failure, so skip the following parameter words and execute the next code word naturally, but with a decoded `extra` parameter.

After the first call to the method, the `executeFn` will point directly to the primitive implementation. If the parameters are valid and the operation can succeed, the primitive will remove self and the parameters, push the result, and return to the caller. If there is a failure, it will pass control to the threaded code, passing an encoded version of the `extra` parameter. The threaded code will never be jitted, which is a small slowdown, but the fallback code is intended to be a relatively rare case.

The `primitives.zig` module has the threaded words for the above, and the linkages to primitives that are described in several modules. To add primitives for a new module, simply add a new module reference to the `modules` array at the beginning of `primitives.zig`:
```zig
const modules = [_]Module{
    Module.init(testModule),
    Module.init(@import("primitives/Object.zig")),
    ...
    Module.init(@import("controlWords.zig").module),
};

```
The `@import`ed module looks like:
```zig
pub const moduleName = "someModuleName";
pub const "somePrimitiveName" = struct {
    pub const number = 60;
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        if (produceValidResultForThisPrimitive(sp)) |result| {
            const newSp = sp.dropPut(result);
            return @call(tailCall, process.check(context.npc.f), .{ context.tpc, newSp, process, context, undefined });
        } else |_| {}
        return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra });
    }
    test "some test" {}
};

```
The `moduleName` and struct name are entered in the module table by name. The `number` const is optional. If it is included, the primitive will also be entered in the table for the numeric version of the primitives lookup, but it must be unique across the whole system. The function must be called `primitive` or `primitiveError` and have this type. The example uses a mythical function to calculate the result of the primitive and if it is successful, returns to the sender with the stack adjusted. If it fails, it passes control to `Extra.primitiveFailed` to continue with the Smalltalk code. The function called `primitive` must not push an error code on the stack. The function called `primitiveError` must push an error code (normally a symbol) on the stack if the primitive fails. Each primitive can have either or both of these functions.