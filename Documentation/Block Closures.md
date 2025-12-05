A block closure is the Smalltalk object syntactically represented as `[ : y | ^ x + y]`. It is evaluated by sending a `value`, `value:`, `value:value:`, etc. message. They are defined in a particular method and may need reference to the `Context` of the defining method if they contain a return (`^`) or to the `ContextData` of the defining method if they reference local or instance variables.

Their general memory layout is:

| Field         | Decription                                                     |
| ------------- | -------------------------------------------------------------- |
| `header`      | normal object header with a class of `BlockClosure`            |
| `code`        | reference to `CompiledBlock` object                            |
| `context`     | reference to the `Context` object                              |
| `contextData` | reference to the `ContextData` object                          |
| ...           | data fields - constant values or locals unique to this closure |
If there is no non-local-return (`^`) in the block, then the `context` field will be `nil`. If there are no references to locals or instance variables, the `contextData` field will be `nil`.

If this is one of the special/immediate block-closure subclasses, the code is pre-defined, so the `code` field is unnecessary:

| Field     | Decription                                          |
| --------- | --------------------------------------------------- |
| `header`  | normal object header with a class of `BlockClosure` |
| `data`    | data fields - constant values                       |
| `context` | reference to the `Context`/`ContextData` object     |
Because these are known to do non-local-returns or not, and to access locals or not, the `context` may actually point to the `ContextData`, or be omitted completely.


If a context is on the stack, allocate closures above it. If it is on the heap, allocate them on the heap. When assigning to local variables via a `Context`/`ContextData`, if the value is a pointer on the stack and it's not immediately above the target `Context` then we need to spill the stack.