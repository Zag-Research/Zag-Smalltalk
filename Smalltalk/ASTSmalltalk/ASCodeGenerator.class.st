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
		'targetClass',
		'maxDepth',
		'inline'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #enumerating }
ASCodeGenerator >> basicBlocksLinearizeDo: aBlock [

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

{ #category : #enumerating }
ASCodeGenerator >> basicBlocksUnorderedDo: aBlock [ 
	basicBlocks do: aBlock
]

{ #category : #'class relationship' }
ASCodeGenerator >> classOf: aString [ 
	self shouldBeImplemented.
]

{ #category : #API }
ASCodeGenerator >> compileBlock: aBlockClosure [
	"compiling of blocks doesn't work right now"

	aBlockClosure manifest ifTrue: [ context compileBlock: aBlockClosure ]


]

{ #category : #accessing }
ASCodeGenerator >> context: anASFileContext [

	context := anASFileContext.

]

{ #category : #API }
ASCodeGenerator >> drop [

	code doDrop; put: #drop
]

{ #category : #initialization }
ASCodeGenerator >> enableInlining [

	inline := true
]

{ #category : #evaluating }
ASCodeGenerator >> eval: aString params: aCollection [

	^ ASCLiteralValue new init: Object new
]

{ #category : #'post processing' }
ASCodeGenerator >> finalAnalysis [


]

{ #category : #API }
ASCodeGenerator >> findTargetClass: aTarget [

^	aTarget = #self
		ifTrue: [ targetClass ]
		ifFalse: [
			aTarget = #super
				ifTrue: [ targetClass superclass ]
				ifFalse: [ nil ] ]
]

{ #category : #accessing }
ASCodeGenerator >> firstBlock [
	^basicBlocks ifNotEmpty: [ basicBlocks at:1] ifEmpty: [ nil ]
]

{ #category : #translating }
ASCodeGenerator >> generate: aMethodOrBlockNode [

	aMethodOrBlockNode zigWalkMethod: self.
	self nameBasicBlocks.
	self tryInlining.
	basicBlocks do: [ :aBasicBlock |
		aBasicBlock markBlockClosuresToManifest.
		maxDepth := maxDepth max: aBasicBlock maxDepth.
		aBasicBlock requiredContextDo: [ :ctxt | ctxt blocks: blocks ] ].
	self halt.
	blocks := blocks select: #manifest.
	blocks do: [ :aBlockClosure | self compileBlock: aBlockClosure ]
]

{ #category : #initialization }
ASCodeGenerator >> initialize [

	super initialize.
	code := ASCMethodBlock nonlocalNameLookupObject: self.
	basicBlocks := OrderedCollection new.
	basicBlocks add: code.
	blocks := OrderedCollection new.
	maxDepth := 0.
	inline := false
]

{ #category : #accessing }
ASCodeGenerator >> inspectBasicBlocks [
	<inspectorPresentationOrder: 0 title: 'ZagBlocks'>
	
	^ SpTreeTablePresenter new
		  roots: basicBlocks;
		  children: [ :aNode | aNode children ];
		  addColumn:
			  (SpStringTableColumn title: 'Blocks' evaluated: #contents);
		  yourself
]

{ #category : #accessing }
ASCodeGenerator >> inspectBasicBlocks [
	<inspectorPresentationOrder: 0 title: 'ZagBlocks'>
	
	^ SpTreeTablePresenter new
		  roots: basicBlocks;
		  children: [ :aNode | aNode children ];
		  addColumn:
			  (SpStringTableColumn title: 'Blocks' evaluated: #contents);
		  yourself
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

{ #category : #serializing }
ASCodeGenerator >> manifest: anASCBlock [

	code makeContextRequired
]

{ #category : #accessing }
ASCodeGenerator >> name [
	^ name
]

{ #category : #'post processing' }
ASCodeGenerator >> nameBasicBlocks [
	basicBlocks doWithIndex: [ :bb :idx |
		bb
			label: (String streamContents: [ :s |
					 s
						 nextPutAll: name;
						 nextPut: $:;
						 print: idx ])
			index: idx ]
]

{ #category : #API }
ASCodeGenerator >> popVariable: aSymbol [

	^ code popVariable: aSymbol
]

{ #category : #API }
ASCodeGenerator >> pushBlock: anASBlock [

	| block |
	block := ASCBlock with: anASBlock.
	blocks add: block.
	code
		doPush: block;
		put: block
]

{ #category : #API }
ASCodeGenerator >> pushLiteral: aLiteral [

	code
		doPush: (ASCLiteralValue new init: aLiteral);
		put: (ASCLiteral with: aLiteral)
]

{ #category : #API }
ASCodeGenerator >> pushSelf [

	| value |
	value := ASCClassValue new init: targetClass.
	code
		doPush: value;
		put: (ASCPushVariable with: (ASLocal new name: #self))
]

{ #category : #API }
ASCodeGenerator >> pushVariable: variable [

	^ code pushVariable: variable
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
ASCodeGenerator >> send: aSymbol targetClass: aTargetClass tail: tail [

	code := code send: aSymbol targetClass: aTargetClass tail: tail.
	basicBlocks add: code.
	aSymbol numArgs + 1 timesRepeat: [
		code doDrop.
].
	code doPush: ASUnknownValue new.

]

{ #category : #API }
ASCodeGenerator >> startMethod: primitiveNumber selector: aSymbol [

	self selector: aSymbol.
	code primitive: primitiveNumber selector: aSymbol 
]

{ #category : #API }
ASCodeGenerator >> storeVariable: anASLocal [

	^ code storeVariable: anASLocal
]

{ #category : #accessing }
ASCodeGenerator >> targetClass [

^	targetClass
]

{ #category : #accessing }
ASCodeGenerator >> targetClass: anObject [ 
targetClass:= anObject 
]

{ #category : #'post processing' }
ASCodeGenerator >> tryInlining [

inline ifFalse: [ ^ self].
	[
	basicBlocks anySatisfy: [ :aBasicBlock |
		aBasicBlock canBeInlinedBy: self ] ] whileTrue
]
