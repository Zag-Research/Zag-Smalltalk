"
I am a code generator.

I take an AST representing a method or a block and turn it into a graph of basic blocks.

Subclasses may do this in sophisticated ways, like agressive inlining and some level of type inference.

When I am done, a caller can access those basic blocks directly, starting from the root, which would be useful for generating CPS style native code.

Alternatively, I can provide a topological sorted list that is usefulfor a strictly linear form such as threaded code.
"
Class {
	#name : #ASCodeGenerator,
	#superclass : #Object,
	#instVars : [
		'context',
		'numLocals',
		'offsets',
		'selector',
		'code',
		'blocks',
		'firstBlock',
		'primitive'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #accessing }
ASCodeGenerator >> context: anASFileContext [

	context := anASFileContext.
	anASFileContext add: self
]

{ #category : #API }
ASCodeGenerator >> drop [

	code drop; put: #drop
]

{ #category : #initialization }
ASCodeGenerator >> initialize [

	super initialize.
	firstBlock := code := ASCBasicBlock new.
	code stack: ASStack new.
	blocks := OrderedCollection new.
	blocks add: code
]

{ #category : #linearize }
ASCodeGenerator >> linearizeTo: anOutput [
primitive =  0 ifTrue: [ anOutput putEmbedded: #verifySelector ] ifFalse: [ anOutput putPrimitive: primitive ].
	blocks do: [ : block | block outputTo: anOutput ]
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
ASCodeGenerator >> popAssign: variable [

	self popLocal: variable 
]

{ #category : #API }
ASCodeGenerator >> popLocal: aSymbol [

	code doDrop; put: (ASCPopLocal with: (offsets at: aSymbol name))
]

{ #category : #API }
ASCodeGenerator >> pushContext [

	self shouldBeImplemented 
]

{ #category : #API }
ASCodeGenerator >> pushLiteral: aLiteral [

	code
		doPush: aLiteral;		put: (ASCLiteral with: aLiteral)
]

{ #category : #API }
ASCodeGenerator >> pushLocal: aSymbol [

	code
		doPush: (self local: aSymbol);
		put: (ASCPushLocal with: (offsets at: aSymbol name))
]

{ #category : #API }
ASCodeGenerator >> pushVariable: variable [

	self pushLocal: variable 
]

{ #category : #API }
ASCodeGenerator >> putEmbedded: aSymbol [

	code put: aSymbol
]

{ #category : #API }
ASCodeGenerator >> returnTop [

	code
		doDrop;
		returnTop
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
ASCodeGenerator >> send: aSymbol [

	code
		doDrop: aSymbol numArgs;
		put: (ASCSend with: aSymbol)
]

{ #category : #API }
ASCodeGenerator >> startMethod: primitiveNumber [
	primitive := primitiveNumber 
]
