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
instVar message1: localVar;message2
```
into:
```mermaid
graph
a
```
