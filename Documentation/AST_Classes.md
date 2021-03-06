##  AST Classes
The AST Classes serve many purposes:
 1. They capture the structure of methods and blocks
 2. They know how to linearize themselves into a sequence of low level operations so they can be interpreted
 3. They know how to generate assembly code for an idealized machine.

```mermaid
classDiagram
ASTNode <|-- ObjectNode
ASTNode <|-- Return
ASTNode <|-- Expression
ASTNode <|-- Dup
ASTNode <|-- Drop
ASTNode <|-- Whitespace
ObjectNode <|-- Class
ObjectNode <|-- Method
Expression <|-- CreateArray
Expression <|-- Store
Store : target
Store : expression
Store --* Target
Store --* expression
Target <|-- Self
Target <|-- Locals
Target <|-- Offset
Offset : target
Offset : offset
Expression <|-- Send
Literal <|-- Symbol
Expression <|-- Literal
Expression <|-- Load
Load : target
Load : offset
Load --* Target
Expression <|-- Block
Block : frameReference
CreateArray : [] expressions
Block --* Method
Send --* Method
Send : receiver
Send : selector
Send : [] arguments
Send : cascade
Send --* Expression
Send --* Send
Symbol o-- Send
Return : expression
Return : frameReference
Return --* Method
Method : class
Method : [] parameters
Method : [] statements
Class : superclass
Class : name
Class : [] methodDict
Class *-- "*" Method
Class --o "1" Symbol
Class : [] classVars
Class : [] sharedPools
ObjectNode : ... instvars
```

### AST Methods

#### `linearize: stream`
This is used to turn the tree structure into a sequence of objects to reflect efficient execution. For example the compiler would convert:
```smalltalk
instVar1 := self message1: localVar with: 42; message2: instVar2
```
into:
```mermaid
graph LR
asn[/:=\]-->|target|y3{{#instVar1}}
asn-->|expression|m1[[message1:with:]]-->|receiver|s2([Self])
m1-->|arg1|y1{{#localVar}}
m1-->|arg2|li{{Literal 42}}
m1-->|cascade|m2[[message2:]]
m2-->|arg1|y2{{#instVar2}}
```

This gets linearized into:
```mermaid
graph LR
s2([Self])-->d1[/dup\]-->l2([Local])-->of2[Offset 1]-->li{{Literal 42}}-->m1[[message1:with:]]-->d2[/drop\]
s3([Self])-->of3[Offset 2]-->m2[[message2:]]-->s1([Self])-->asn[/:= 1\]
```
