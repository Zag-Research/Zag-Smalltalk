"
I generate Zig code equivalent to the built program captured by an ASCompiler
"
Class {
	#name : #ASZigGenerator,
	#superclass : #Object,
	#instVars : [
		'compiler',
		'stream'
	],
	#classVars : [
		'Symbols'
	],
	#category : #'ASTSmalltalk-Compiler'
}

{ #category : #'class initialization' }
ASZigGenerator class >> initialize [
	Symbols := Set withAll: #(valueWithArguments: cull: cull:cull: cull:cull:cull: cull:cull:cull:cull: value value: value:value: value:value:value: value:value:value:value: self Object BlockClosure False True UndefinedObject SmallInteger Symbol Character Float Array String Class Metaclass Behavior Method Magnitude Number System Return Send Literal Load Store SymbolTable Dispatch yourself == ~~ ~= = + - * size ClassTable)
]

{ #category : #accessing }
ASZigGenerator >> compiler: anASPharoCompiler [ 
	compiler := anASPharoCompiler
]

{ #category : #accessing }
ASZigGenerator >> stream: aWriteStream [ 
	stream := aWriteStream
]

{ #category : #visiting }
ASZigGenerator >> symbolRef: aSymbol [
	^ '_symbol.',((Symbols includes: aSymbol) ifTrue: [ aSymbol ] ifFalse: [ 'foo' ])asZigName
]

{ #category : #visiting }
ASZigGenerator >> visit: anASTNode [ 
	^ anASTNode printOn: stream
	"^ anASTNode acceptVisitor: self"
]

{ #category : #visiting }
ASZigGenerator >> visitAll: aCollection [
	aCollection  do: [ :node | self visit: node ]
]

{ #category : #visiting }
ASZigGenerator >> visitBlockNode: anASBlock [
	anASBlock printOn: stream
]

{ #category : #visiting }
ASZigGenerator >> visitLiteral: anASLiteral [ 
	anASLiteral literal printOn: stream
]

{ #category : #visiting }
ASZigGenerator >> visitMethodNode: anASMethodNode [

	stream << 'fn ' << anASMethodNode selector asZigName
	<< '(thread : *Thread, self: Object) MethodReturns {'.
	self visitAll: anASMethodNode body.
	stream << '}'
]

{ #category : #visiting }
ASZigGenerator >> visitReturn: anASReturn [
	self visit: anASReturn expression.
	self flag: #todo.
	stream << 'return MethodReturns.Normal'
]

{ #category : #visiting }
ASZigGenerator >> visitSelfNode: anASSelf [ 
	stream << 'thread.push(thread.stack()[0])'
]

{ #category : #visiting }
ASZigGenerator >> visitSend: anASSend [ 
	self visit: anASSend target.
	self visitAll: anASSend args.
	stream << 'call(&thread,thread.stack()[0],'<< (self symbolRef: anASSend selector) <<');'
]

{ #category : #visiting }
ASZigGenerator >> visitVariable: anASRef [
	anASRef printOn: stream
]
