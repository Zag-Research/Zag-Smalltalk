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
		'firstBlock',
		'primitive',
		'basicBlocks',
		'blocks',
		'breakAtCall',
		'name'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #API }
ASCodeGenerator >> break [

	code := code break.
	basicBlocks add: code.
]

{ #category : #accessing }
ASCodeGenerator >> breakAtCall [
	breakAtCall := true
]

{ #category : #linearize }
ASCodeGenerator >> cleanupAndGetPrimitive [

	self finalAnalysis.
	^ primitive = 0
		  ifTrue: [ nil ]
		  ifFalse: [ primitive ]
]

{ #category : #API }
ASCodeGenerator >> compileBlock: anASCBlock [

	blocks add: anASCBlock .
	" also schedule as componet"
]

{ #category : #accessing }
ASCodeGenerator >> context: anASFileContext [

	context := anASFileContext.

]

{ #category : #API }
ASCodeGenerator >> drop [

	code drop; put: #drop
]

{ #category : #linearize }
ASCodeGenerator >> finalAnalysis [

	basicBlocks doWithIndex: [ :bb : idx |
		bb label: (String streamContents: [:s| s nextPutAll: name;nextPut: $:;print: idx]).
		bb isUnfinished ifTrue: [ bb put: #returnSelf ] ]
]

{ #category : #initialization }
ASCodeGenerator >> initialize [

	super initialize.
	firstBlock := code := ASCBasicBlock new.
	code stack: ASStack new.
	basicBlocks := OrderedCollection new.
	basicBlocks add: code.
	blocks := OrderedCollection new.
	breakAtCall := false
]

{ #category : #linearize }
ASCodeGenerator >> linearizeBasicBlocksDo: aBlock [

	| rest current |
	current := basicBlocks first.
	rest := basicBlocks allButFirst asSet.
	[ true ] whileNotNil: [
		aBlock value: current.
		current else
			ifNil: [
				rest isEmpty ifTrue: [ ^ self ].
				current := rest atRandom ifNil: [ ^ self ] ]
			ifNotNil: [ :next | current := next ].
		rest remove: current ].
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
ASCodeGenerator >> pushBlock: anASBlock [

	| block |
	block := ASCBlock with: anASBlock.
	self compileBlock: block.
	code
		doPush: block;
		put: block
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

	selector := aSymbol. 	name := context name
]

{ #category : #API }
ASCodeGenerator >> send: aSymbol [

	| doBreak |
	doBreak := context checkBreakAtCall: aSymbol.
	code
		doDrop: aSymbol numArgs;
		put: (ASCSend with: aSymbol and: doBreak).
	doBreak ifTrue: [ self break ]
]

{ #category : #API }
ASCodeGenerator >> startMethod: primitiveNumber [
	primitive := primitiveNumber 
]

{ #category : #enumerating }
ASCodeGenerator >> unorderedBasicBlocksDo: aBlock [ 
	basicBlocks do: aBlock
]
