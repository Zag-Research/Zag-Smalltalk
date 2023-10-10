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
		'selector',
		'code',
		'basicBlocks',
		'blocks',
		'name',
		'localVariables',
		'depth'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #API }
ASCodeGenerator >> compileBlock: anASCBlock [

	blocks add: anASCBlock .
	" also schedule as componet"
]

{ #category : #accessing }
ASCodeGenerator >> context: anASFileContext [

	context := anASFileContext.

]

{ #category : #accessing }
ASCodeGenerator >> depth: anInteger [ 
	depth := anInteger
]

{ #category : #API }
ASCodeGenerator >> drop [

	code drop; put: #drop
]

{ #category : #evaluating }
ASCodeGenerator >> eval: aString params: aCollection [

	^ ASCLiteralValue new init: Object new
]

{ #category : #linearize }
ASCodeGenerator >> finalAnalysis [

	basicBlocks doWithIndex: [ :bb :idx |
		bb
			label: (String streamContents: [ :s |
					 s
						 nextPutAll: name;
						 nextPut: $:;
						 print: idx ])
			index: idx.
		]
]

{ #category : #initialization }
ASCodeGenerator >> initialize [

	super initialize.
	code := ASCMethodBlock new.
	basicBlocks := OrderedCollection new.
	basicBlocks add: code.
	blocks := OrderedCollection new.
	depth := 0
]

{ #category : #linearize }
ASCodeGenerator >> linearizeBasicBlocksDo: aBlock [

	| rest current |
	current := basicBlocks first.
	rest := basicBlocks allButFirst asSet.
	[ true ] whileNotNil: [
		aBlock value: current.
		current next
			ifNil: [
				rest isEmpty ifTrue: [ ^ self ].
				current := rest atRandom ifNil: [ ^ self ] ]
			ifNotNil: [ :next | current := next ].
		rest remove: current ].
]

{ #category : #accessing }
ASCodeGenerator >> locals: locals arguments: arguments [

	localVariables := OrderedCollection new.
	localVariables add: ASCSelf new.
	arguments doWithIndex: [ :each :index |
		localVariables add: (ASCParameter name: each offset: index) ].
	locals doWithIndex: [ :each :index |
		localVariables add:
			(ASCLocal name: each offset: index + arguments size) ]
]

{ #category : #compiling }
ASCodeGenerator >> lookupLocal: aSymbol [

	^	localVariables detect: [ :variable |
		aSymbol = variable name ]
]

{ #category : #accessing }
ASCodeGenerator >> lookupObject: anObject [

	^ context lookupObject: anObject
]

{ #category : #API }
ASCodeGenerator >> popAssign: variable [

	self popLocal: (variable lookup: self)
]

{ #category : #API }
ASCodeGenerator >> popLocal: aSymbol [

	code
		put:
			(ASCPopLocal with:
					 ((localVariables at: aSymbol offset+1) store: code top at: depth));
		doDrop
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

| value |
value:= ASCLiteral with: aLiteral.
	code
		doPush: value;		put: value
]

{ #category : #API }
ASCodeGenerator >> pushLocal: aSymbol [

	| local |
	local := aSymbol lookup: self.
	code
		doPush: local;
		put: (ASCPushLocal with: (local referencedAt:  depth))
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

	code := (code send: aSymbol with: self).
	basicBlocks add: code
]

{ #category : #API }
ASCodeGenerator >> startMethod: primitiveNumber [

	code primitive: primitiveNumber
]

{ #category : #enumerating }
ASCodeGenerator >> unorderedBasicBlocksDo: aBlock [ 
	basicBlocks do: aBlock
]
