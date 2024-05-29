"
I reperesent an element of a stack within a method.

Initially this will simply retain the values.

Eventually it will implement Phi nodes for SSA evaluation, supporting some level of type inference

Code generators can inquire as to values and types
"
Class {
	#name : 'ASStackElement',
	#superclass : 'ASStack',
	#instVars : [
		'top',
		'llvmName',
		'source'
	],
	#classVars : [
		'LLVMNumber'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'initialization' }
ASStackElement class >> reset [
	LLVMNumber := 0
]

{ #category : 'API' }
ASStackElement >> addBlockClosure: aCollection [ 
	self shouldBeImplemented.
]

{ #category : 'accessing' }
ASStackElement >> at: aSymbol put: anObject [
	rest at: aSymbol put: anObject 
]

{ #category : 'accessing' }
ASStackElement >> depth [

	^ 1 + rest depth
]

{ #category : 'accessing' }
ASStackElement >> drop [
	^ rest
]

{ #category : 'removing' }
ASStackElement >> drop: count [ 
	count = 0 ifTrue: [ ^self ].
	^ rest drop: count-1
]

{ #category : 'accessing' }
ASStackElement >> find: aSymbol [
	^ rest find: aSymbol
]

{ #category : 'adding' }
ASStackElement >> importedSource: anInteger [

	^ (self class fromStack: (rest importedSource: anInteger + 1)) top: top; source: anInteger
]

{ #category : 'testing' }
ASStackElement >> isEmpty [

	^ false
]

{ #category : 'accessing' }
ASStackElement >> llvmName [

	^ llvmName ifNil: [
		  llvmName := (LLVMNumber := LLVMNumber + 1) asString ]
]

{ #category : 'accessing' }
ASStackElement >> llvmNameReset [

	llvmName := nil
]

{ #category : 'accessing' }
ASStackElement >> llvmNewName [

	^ self
		  nameReset;
		  name
]

{ #category : 'API' }
ASStackElement >> makeContextRequired [
	rest := rest makeContextRequired.
	^ self
]

{ #category : 'API' }
ASStackElement >> offset: anInteger namedContext: blockLocal stack: blockStack [

	^ rest
		  offset: anInteger + 1
		  namedContext: blockLocal
		  stack: blockStack
]

{ #category : 'API' }
ASStackElement >> offset: anInteger to: anObject [

	^ rest
		  offset: anInteger + 1
		  to: anObject
]

{ #category : 'API' }
ASStackElement >> offset: anInteger to: anObject context: blockLocal stack: blockStack other: blockOther [

	^ rest
		  offset: anInteger + 1
		  to: anObject
		  context: blockLocal
		  stack: blockStack
		  other: blockOther
]

{ #category : 'enumerating' }
ASStackElement >> requiredContextDo: aBlock [ 
	^ rest requiredContextDo: aBlock
]

{ #category : 'initialization' }
ASStackElement >> reset [

	^ rest reset
]

{ #category : 'accessing' }
ASStackElement >> source [

	^ source
]

{ #category : 'accessing' }
ASStackElement >> source: anObject [

	source := anObject
]

{ #category : 'accessing' }
ASStackElement >> top [
^ top
]

{ #category : 'accessing' }
ASStackElement >> top: value [

"value assertIsValue ."
	top := value
]
