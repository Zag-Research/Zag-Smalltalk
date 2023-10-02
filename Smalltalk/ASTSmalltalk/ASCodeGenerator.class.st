"
I am an abstract code generator. My subclasses are used for several purposes.

There are text generators that are used to create text files (currently only in Zig) that can be compiled with the Zag runtime to generate standalone code.

There will be a runtime code generator written in Zig that will generate threaded or JIT code at runtime.
"
Class {
	#name : #ASCodeGenerator,
	#superclass : #Object,
	#instVars : [
		'context',
		'maxDepth',
		'stack',
		'numLocals',
		'offsets',
		'selector'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #accessing }
ASCodeGenerator >> context: anASFileContext [

	context := anASFileContext.
	anASFileContext add: self
]

{ #category : #stackAccessHelpers }
ASCodeGenerator >> doCall: aSymbol [

	stack := stack drop: aSymbol numArgs
]

{ #category : #stackAccessHelpers }
ASCodeGenerator >> doPop [
|result|
	result := stack value.
	stack := stack pop.
	^ result
]

{ #category : #stackAccessHelpers }
ASCodeGenerator >> doPush: anObject [

	stack := stack push: anObject.
	maxDepth := maxDepth max: stack depth
]

{ #category : #API }
ASCodeGenerator >> drop [

	self subclassResponsibility
]

{ #category : #initialization }
ASCodeGenerator >> initialize [

	super initialize.
	maxDepth := 0.
	stack := ASStack new 
]

{ #category : #'local field support' }
ASCodeGenerator >> local: aString [

	^ Object new
]

{ #category : #accessing }
ASCodeGenerator >> locals: locals arguments: arguments [

	numLocals := locals size.
	offsets := Dictionary new.
	locals reversed , arguments reversed , #( self ) doWithIndex: [
		:each
		:index | offsets at: each put: index ]
]

{ #category : #accessing }
ASCodeGenerator >> lookupObject: anObject [
	^ context lookupObject: anObject 
]

{ #category : #API }
ASCodeGenerator >> popLocalData: name stackIndex: stackIndex blockIndex: blockIndex [

	self subclassResponsibility
]

{ #category : #API }
ASCodeGenerator >> primitiveOrVerify: primitiveNumber [

	self subclassResponsibility
]

{ #category : #API }
ASCodeGenerator >> pushContext [
	self subclassResponsibility 
]

{ #category : #API }
ASCodeGenerator >> pushLiteral: aLiteral [

	self subclassResponsibility
]

{ #category : #API }
ASCodeGenerator >> pushLocal: aSelector [

	self subclassResponsibility
]

{ #category : #API }
ASCodeGenerator >> putEmbedded: aSelector [

	self subclassResponsibility
]

{ #category : #API }
ASCodeGenerator >> putPrimitive: aSelector [

	self subclassResponsibility
]

{ #category : #API }
ASCodeGenerator >> returnTop [

	self subclassResponsibility
]

{ #category : #accessing }
ASCodeGenerator >> selector [

	^	selector
]

{ #category : #accessing }
ASCodeGenerator >> selector: aSymbol [
	selector := aSymbol 
]

{ #category : #API }
ASCodeGenerator >> send: aSelector [


	self subclassResponsibility
]
