Threaded Words or Threaded Functions or Code Words are the functions that are pointed to by the threaded part of a `CompiledMethod`.

# ThreadedFns
The following threaded functions are defined. In the stack diagrams, TOS is to the right. The stack to the left of the arrow is what is expected (with unspecified values below it), and the stack to the right is the result with the unspecified values unaffected unless mentioned in the description. 

---
## `array`
##### parameters:  size 
##### stack: on ➛ array

allocate an initialized array from `size` objects on the stack (in reverse order). If any of the values are `thisContext` or a `BlockClosure` then either the array needs to be allocated on the stack or the stack needs to be spilled.

---
## `asThunk`
##### stack: value ➛ closure

a thunk that returns the value. The thunk will be immediate, or allocated on the stack

---
## `branch`
##### parameters: address

 unconditional branch to the address in the current method

---
## `classCase`
##### parameters: classDescriptor addresses
##### stack: o ➛

match class of the object and branch to corresponding address

---
## `cullColon`
##### stack: r o1 ➛ o2

optimization of `send` with the selector `cull:`

---
## `drop`
##### stack: o ➛

discard TOS

---
## `dup`
##### stack: o1 ➛ o1 o1

duplicate TOS

---
## `inlinePrimitive`
##### parameters: symbol + primitive#
##### stack: r os ➛ o2

evaluate the primitive, or if it fails, send the symbol
see the [[Primitives]] for more information

---
## `inlinePrimitiveModule`
##### parameters: primitiveName primitiveModule
##### stack: r os ➛ o2

? don't know where to get the selector ?
same as [[#`inlinePrimitive`]] except named

---
## `over`
##### stack: o1 o2 ➛ o1 o2 o1

copies the next on stack

---
## `pop`
##### parameters: variableDescriptor
##### stack: o ➛

pops TOS to the variable

---
## `popAssociationValue`
##### parameters: associationAddress
##### stack: o ➛

pops TOS to the value field of the `Association`

---
## `primitive`
##### parameters: primitive#
##### stack: r on ➛ o2 | r on

evaluate the primitive and return; continuing this method on failure
see the [[Primitives]] for more information

---
## `primitiveError`
##### parameters: primitive#
##### stack: r on ➛ o2 | r on e

same as `primitive` except pushes error on failure
see the [[Primitives]] for more information

---
## `primitiveModule`
##### parameters: primitiveName primitiveModule
##### stack: r on ➛ o2 | r on

evaluate the primitive and return; continuing this method on failure
see the [[Primitives]] for more information

---
## `primitiveModuleError`
##### parameters: primitiveName primitiveModule
##### stack: r on ➛ o2 | r on e

same as `primitiveModule` except pushes error on failure
see the [[Primitives]] for more information

---
## `push`
##### parameter: variableDescriptor
see [[#parameters variableDescriptor|pop]] for description
##### stack: ➛ o

push the variable onto the stack

---
## `pushAssociationValue`
##### parameter: associationAddress
##### stack: ➛ o

push the value field of the `Association` onto the stack

---
## `pushClosure`
##### parameter: closureDescriptor compiledClosure
##### stack: os ➛ o

creates a block closure; closureDescriptor is a tagged integer: number of fields (low 8 bits), flag to include context, flag to include contextData

---
## `pushLiteral`
##### parameter: immutableObject
##### stack: ➛ o

push the literal onto the stack

---
## `pushThisContext`
##### stack: ➛ o

push the context onto the stack

---
## `pushThisProcess`
##### stack: ➛ o

push the process onto the stack

---
## `returnSelf`
##### stack: self ... ➛ self

return to the caller, with `self` as the result

---
## `returnTop`
##### stack: self ... o ➛ o

return to the caller, with TOS as the result

---
## `returnTopNonLocal`
##### stack: ... o ➛ o

return from the method that created the current closure

---
## `send`
##### parameter: selector
##### stack: r os ➛ o

evaluates the receiver with parameters, replacing them with the result

---
## `store`
##### parameter: variableDescriptor
see [[#parameters variableDescriptor|pop]] for description
##### stack: o ➛ o

stores TOS to the variable, leaving it on the stack

---
## `tailSend`
##### parameter: selector
##### stack: r os ➛ o

like `send` except result to our sender; no inline return

---
## `value`
##### stack: r ➛ o

optimization of `send` with the selector `value`

---
## `valueColon`
##### stack: r o1 ➛ o2

optimization of `send` with the selector `value:`
