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
ASCodeGenerator >> assignLocal: anASLocal [

	code assignLocal: anASLocal
]

{ #category : #API }
ASCodeGenerator >> compileBlock: anASCBlock [


	blocks add: anASCBlock.
	context compileBlock: anASCBlock
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

	code doDrop; put: #drop
]

{ #category : #evaluating }
ASCodeGenerator >> eval: aString params: aCollection [

	^ ASCLiteralValue new init: Object new
]

{ #category : #linearize }
ASCodeGenerator >> finalAnalysis [
"self halt."
	basicBlocks doWithIndex: [ :bb :idx |
		bb
			label: (String streamContents: [ :s |
					 s
						 nextPutAll: name;
						 nextPut: $:;
						 print: idx ])
			index: idx ]
]

{ #category : #translating }
ASCodeGenerator >> generate: aMethodOrBlockNode [

	| methodContext |
	aMethodOrBlockNode zigWalk: self.
	methodContext := basicBlocks first findContext.
	methodContext blockClosures do: [ :block | self compileBlock: block ] 
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

	context forClass ifNotNil: [ :theClass |
		code doPush: (ASCClassValue new init: theClass) ].
	code locals: locals arguments: arguments
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

{ #category : #accessing }
ASCodeGenerator >> name [
	^ name
]

{ #category : #API }
ASCodeGenerator >> popAssign: variable [

	self popLocal: variable
]

{ #category : #API }
ASCodeGenerator >> popLocal: aSymbol [

	code popLocal: aSymbol
]

{ #category : #API }
ASCodeGenerator >> pushBlock: anASBlock [

	| block |
	block := ASCBlock with: anASBlock.
	code doPush: block;put: block
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

	code pushLocal: aSymbol
]

{ #category : #API }
ASCodeGenerator >> pushSelf [

		| value |
	value := ASCSelf new.
	code
		doPush: value;
		put: value
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

	selector := aSymbol.
	name := context name.
	code selector: aSymbol 
]

{ #category : #API }
ASCodeGenerator >> send: aSymbol target: aTarget [

	| numArgs params target |
	numArgs := aSymbol numArgs.
	params := Array new: numArgs.
	target := aTarget isSymbol
		          ifTrue: [ aTarget ]
		          ifFalse: [ nil ].
	"	[ numArgs > 0 ] whileTrue: [
		params at: numArgs put: stack top.
		self doDrop.
		numArgs := numArgs - 1 ].
	stack top: (aGenerator eval: aSymbol params: params)."
	code := code send: aSymbol target: target tail: false.
	basicBlocks add: code
]

{ #category : #API }
ASCodeGenerator >> send: aSymbol target: aTarget tail: tail [

	| numArgs params target |
	numArgs := aSymbol numArgs.
	params := Array new: numArgs.
	target := aTarget isSymbol
		          ifTrue: [ aTarget ]
		          ifFalse: [ nil ].
	"	[ numArgs > 0 ] whileTrue: [
		params at: numArgs put: stack top.
		self doDrop.
		numArgs := numArgs - 1 ].
	stack top: (aGenerator eval: aSymbol params: params)."
	code := code send: aSymbol target: target tail: tail.
	basicBlocks add: code
]

{ #category : #API }
ASCodeGenerator >> startMethod: primitiveNumber selector: aSymbol [

	self selector: aSymbol.
	code primitive: primitiveNumber selector: aSymbol 
]

{ #category : #enumerating }
ASCodeGenerator >> unorderedBasicBlocksDo: aBlock [ 
	basicBlocks do: aBlock
]
