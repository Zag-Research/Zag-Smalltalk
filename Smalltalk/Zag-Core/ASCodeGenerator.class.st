"
I am a code generator.

I take an AST representing a method or a block and turn it into a graph of basic blocks.

Subclasses may do this in sophisticated ways, like agressive inlining and some level of type inference.

When I am done, a caller can access those basic blocks directly, starting from the root, which would be useful for generating CPS style native code.

Alternatively, I can provide a topological sorted list that is usefulfor a strictly linear form such as threaded code.
"
Class {
	#name : 'ASCodeGenerator',
	#superclass : 'Object',
	#instVars : [
		'code',
		'basicBlocks',
		'name',
		'localVariables',
		'targetClass',
		'inline',
		'tailCallOptimize',
		'blockClosures',
		'parentCodeGenerator',
		'enclosingScope',
		'returnBlock',
		'blocksWithCalls',
		'currentCodeUnit',
		'nextSequence',
		'compiledUnits',
		'blockNumber'
	],
	#category : 'Zag-Core-CodeGen',
	#package : 'Zag-Core',
	#tag : 'CodeGen'
}

{ #category : 'API' }
ASCodeGenerator >> addBlock: aBasicBlock [

	aBasicBlock label: self blockLabel.
	^ basicBlocks add: aBasicBlock
]

{ #category : 'accessing' }
ASCodeGenerator >> basicBlocks [
	^ basicBlocks
]

{ #category : 'enumerating' }
ASCodeGenerator >> basicBlocksLinearizeDo: aBlock [

	| rest current |
	current := basicBlocks first.
	rest := basicBlocks allButFirst asSet.
	[ true ] whileNotNil: [
		aBlock value: current.
		current nextBasicBlocks
			ifNil: [
				rest isEmpty ifTrue: [ ^ self ].
				current := rest anyOne ifNil: [ ^ self ] ]
			ifNotNil: [ :next | current := next ].
		rest remove: current ]
]

{ #category : 'enumerating' }
ASCodeGenerator >> basicBlocksLocalizeDo: aBlock [

	| rest current namedBlock |
	namedBlock := current := basicBlocks first.
	rest := basicBlocks allButFirst asSet.
	[ true ] whileTrue: [
		aBlock value: current.
		current nextBasicBlocks ifNotNil: [ :next | current := next ] ifNil: [
			rest isEmpty ifTrue: [ ^ self ].
			current := rest
				           detect: [ :each | each namedBlock = namedBlock ]
				           ifFound: #yourself
				           ifNone: [
				           rest
					           detect: #isNamedBlock
					           ifFound: #yourself
					           ifNone: [ ^ self ] ] ].
		rest remove: current ]
]

{ #category : 'enumerating' }
ASCodeGenerator >> basicBlocksUnorderedDo: aBlock [ 
	basicBlocks do: aBlock
]

{ #category : 'API' }
ASCodeGenerator >> blockClosureClass [

	^ currentCodeUnit blockClosureClass
]

{ #category : 'accessing' }
ASCodeGenerator >> blockLabel [

	^ (String streamContents: [ :s |
			 s
				 nextPutAll: basicBlocks first label;
				 nextPut: $.;
				 print: (blockNumber := blockNumber + 1) ])
]

{ #category : 'API' }
ASCodeGenerator >> branch: aBasicBlock [

	^ aBasicBlock ifNotNil: [ code branch: aBasicBlock ]
]

{ #category : 'class relationship' }
ASCodeGenerator >> classOf: aString [ 
	self shouldBeImplemented.
]

{ #category : 'compiling' }
ASCodeGenerator >> compileBlock: anAStBlock [

	code := ASCCompiledClosureBlock new.
	compiledUnits add: code.
	anAStBlock compiledUnit: code.
	code initNamedIndirect: basicBlocks first.
	anAStBlock ast zigWalkMethod: self
]

{ #category : 'finalAnalysis' }
ASCodeGenerator >> compileRemainingBlockClosures [

	blockClosures do: [ : blockClosure | blockClosure compileIn: self ]
]

{ #category : 'finalAnalysis' }
ASCodeGenerator >> createContextWhereRequired [

	basicBlocks first
		propagateContextFreeFor.
	^ basicBlocks	do: #defaultAsReturn;
		do: #requireSourceContext;
		do: #setInContextForOperations
]

{ #category : 'API' }
ASCodeGenerator >> drop [

	code addOperation: ASCDrop new
]

{ #category : 'API' }
ASCodeGenerator >> dup [

	code addOperation: (ASCDup new)
]

{ #category : 'initialization' }
ASCodeGenerator >> enableInlining [

	self inline: true
]

{ #category : 'accessing' }
ASCodeGenerator >> enclosingScope: aScope [
	enclosingScope := aScope
]

{ #category : 'finalAnalysis' }
ASCodeGenerator >> finalAnalysis [

	self inlineWherePossible.
	self removeUnusedBlockClosures.
	self compileRemainingBlockClosures.
	self optimizeLocalVariableLocations.
	self removeUnusedCode.
	self inlineDependentPrimitives.
	self createContextWhereRequired.
	self reifyLocals
]

{ #category : 'data flow instructions' }
ASCodeGenerator >> flowAnalysis [

	basicBlocks do: #addFlow
]

{ #category : 'translating' }
ASCodeGenerator >> generateFrom: aMethodOrBlockNode [

	code initSelf: targetClass.
	aMethodOrBlockNode zigWalkMethod: self
]

{ #category : 'as yet unclassified' }
ASCodeGenerator >> generateTemp [

	^ code nextSequence
]

{ #category : 'accessing' }
ASCodeGenerator >> getOrMakeReturnBlock [

	^ code nextBasicBlocks ifNil: [
		  | tempBlock |
		  tempBlock := self newInline.
		  tempBlock branchedToFrom: code.
		  tempBlock returnTop.
		  tempBlock ]
]

{ #category : 'initialization' }
ASCodeGenerator >> initCode [

	code := ASCCompiledMethodBlock new
]

{ #category : 'initialization' }
ASCodeGenerator >> initialize [

	super initialize.
	self initCode.
	basicBlocks := OrderedCollection with: code.
	compiledUnits := OrderedCollection with: code.
	blockClosures := OrderedCollection new.
	blocksWithCalls := Set new.
	inline := nextSequence := blockNumber := 0.
	tailCallOptimize := true
]

{ #category : 'accessing' }
ASCodeGenerator >> inline: aBooleanOrInteger [

	inline := aBooleanOrInteger isInteger
		          ifTrue: [ aBooleanOrInteger ]
		          ifFalse: [ aBooleanOrInteger ifTrue: [ 1000 ] ifFalse: [ 0 ] ]
]

{ #category : 'finalAnalysis' }
ASCodeGenerator >> inlineDependentPrimitives [
]

{ #category : 'inlining' }
ASCodeGenerator >> inlinePrimitive: aCompiledMethod for: aClass from: anASCSend [

	| primitive |
	primitive := aCompiledMethod primitive.
	returnBlock := self getOrMakeReturnBlock.
	self halt.
	(primitive between: 201 and: 210) ifTrue: [ ^ self inlineValue: (aCompiledMethod numArgs) for: aClass next: returnBlock ].
	code replaceOperation: (ASCEmbed new
			 selector: aCompiledMethod selector;
			 primitive: primitive).
	self branch: returnBlock
]

{ #category : 'inlining' }
ASCodeGenerator >> inlineRecursiveMethod: aCompileMethod block: anASCInlinedMethodBlock from: anASCSend [

	anASCSend isTailcall ifFalse: [ ^ self ].
	code removeLastOperation.
	code popStackTo: anASCInlinedMethodBlock.
	self branch: anASCInlinedMethodBlock
]

{ #category : 'inlining' }
ASCodeGenerator >> inlineValue: anOffset for: aClass next: aBasicBlock [

	aClass == ZagBlockClosure ifTrue: [ ^ self inlineBlock: anOffset next: aBasicBlock ].
	self halt " handle Symbol value: "
]

{ #category : 'finalAnalysis' }
ASCodeGenerator >> inlineWherePossible [

	| temp remainingInlines |
	remainingInlines := inline.
	[
	remainingInlines > 0 ifFalse: [ ^ self ].
	remainingInlines := remainingInlines - 1.
	blocksWithCalls isEmpty ] whileFalse: [
		temp := blocksWithCalls.
		blocksWithCalls := Set new.
		temp do: [ :block |
			code := block.
			block inlineIfPossible: self ] ].
	"Daniel remove this for now"
	"basicBlocks do: [ :block | block inlineWithFixedCFG: self ]"
]

{ #category : 'accessing' }
ASCodeGenerator >> inspectBasicBlocks [

	<inspectorPresentationOrder: 0 title: 'ZagBlocks'>
	^ SpTreeTablePresenter new
		  roots: self zagBlocks;
		  children: [ :aNode | aNode children ];
		  addColumn:
			  (SpStringTableColumn title: 'Blocks' evaluated: #contents);
		  yourself
]

{ #category : 'accessing' }
ASCodeGenerator >> locals: locals parameters: arguments [

	^ currentCodeUnit locals: locals parameters: arguments
]

{ #category : 'compiling' }
ASCodeGenerator >> lookupLocal: aSymbol [

	^	localVariables detect: [ :variable |
		aSymbol = variable name ]
]

{ #category : 'accessing' }
ASCodeGenerator >> lookupObject: anObject [

	^ self incompleteImplementation
]

{ #category : 'private - testing' }
ASCodeGenerator >> methodBlock [

	^ basicBlocks ifNotEmpty: [ basicBlocks first ] ifEmpty: [ nil ]
]

{ #category : 'accessing' }
ASCodeGenerator >> name [
	^ name
]

{ #category : 'private - testing' }
ASCodeGenerator >> newInline [

	^ self addBlock: ASCReturnBlock new
]

{ #category : 'private - testing' }
ASCodeGenerator >> newInlineBlockClosure [

	^ self addBlock: (currentCodeUnit newInlinedBlock: nil)
]

{ #category : 'instance creation' }
ASCodeGenerator >> newInlinedBlock: aMethodAST [

	^ self addBlock: (code newInlinedBlock: aMethodAST)
]

{ #category : 'API' }
ASCodeGenerator >> nextSequence [

	^ nextSequence := nextSequence + 1
]

{ #category : 'API' }
ASCodeGenerator >> offset: anInteger to: anObject context: blockLocal stack: blockStack other: blockOther [
	^ code offsetOf: anObject context: blockLocal stack: blockStack other: blockOther
]

{ #category : 'API' }
ASCodeGenerator >> offsetOf: anObject context: blockLocal stack: blockStack other: blockOther [
	^ code offsetOf: anObject context: blockLocal stack: blockStack other: blockOther
]

{ #category : 'API' }
ASCodeGenerator >> offsetTo: anObject methodBlock: aBlock [
	^ code offsetTo: anObject methodBlock: aBlock 
]

{ #category : 'finalAnalysis' }
ASCodeGenerator >> optimizeLocalVariableLocations [
	"drive the ownership of compilable unit blocks"

	| varsModified varsReferenced nonLocalReturnBlock |
	varsModified := IdentityDictionary new.
	varsReferenced := IdentityDictionary new.

	"get each block to gather variable usage"
	compiledUnits do: [ :bb |
		bb deriveVariableReferences: varsReferenced modifies: varsModified ].
	"
	1. If a variable is only referenced in the method, it will be put in the Context (or just on the stack if no context is created).
   2. If the variable is only referenced in one BlockClosure then it will be created as a local variable there.
   3. For values referenced in two or more places, modified in at least one, the default would be to put them in the Context. However, if a BlockClosure has a reference to the Context and the closure gets moved to the heap, it will drag the entire stack with it. Therefore the only closures that reference the context will be ones with non-local returns (or that create closures that need a context reference). Variables referenced in non-local-return closures will be placed in the context.
   4. All other variables will be placed in a closure that modifies the variable.
"
	nonLocalReturnBlock := [ :dictionary :var |
	                       ((dictionary at: var) asSet
		                        detect: [ :bb | bb hasNonLocalReturn ]
		                        ifNone: [ nil ])
		                       ifNotNil: [ self methodBlock owns add: var ]
		                       ifNil: [
		                       (dictionary at: var) anyOne owns add: var ] ].

	varsModified keys , varsReferenced keys do: [ :var |
		(varsModified includesKey: var)
			ifTrue: [ nonLocalReturnBlock value: varsModified value: var ]
			ifFalse: [
				| blocksWithOccurences |
				blocksWithOccurences := varsReferenced at: var.
				(blocksWithOccurences sizeAsSet = 1 and: [
					 blocksWithOccurences anyOne hasNonLocalReturn not ])
					ifTrue: [ "only one reference so put the variable there"
						blocksWithOccurences anyOne owns add: var ]
					ifFalse: [ nonLocalReturnBlock value: varsReferenced value: var ] ] ]
]

{ #category : 'code generation' }
ASCodeGenerator >> outputTo: output [

	self methodBlock outputTo: output
]

{ #category : 'accessing' }
ASCodeGenerator >> parentCodeGenerator: aCodeGenerator [

	aCodeGenerator ifNotNil: [
		parentCodeGenerator := aCodeGenerator ]
]

{ #category : 'API' }
ASCodeGenerator >> popVariable: aSymbol [

	^ code popVariable: aSymbol
]

{ #category : 'API' }
ASCodeGenerator >> primitive: primitiveNumber [

	code primitive: primitiveNumber
]

{ #category : 'API' }
ASCodeGenerator >> pushArray: arraySize [

	^ code pushArray: arraySize
]

{ #category : 'API' }
ASCodeGenerator >> pushBlock: anASBlockNode [

	| block |
	block := AStBlock new block: anASBlockNode.
	blockClosures add: block.
	code addOperation: (ASCBlock with: block)
]

{ #category : 'API' }
ASCodeGenerator >> pushLiteral: aLiteral [

	^ code pushLiteral: aLiteral
]

{ #category : 'API' }
ASCodeGenerator >> pushSelf [

	^ code pushVariable: #self
]

{ #category : 'API' }
ASCodeGenerator >> pushVariable: variable [

	^ code pushVariable: variable
]

{ #category : 'testing' }
ASCodeGenerator >> recursiveBasicBlock: aMethodAST [

	self flag: ' need to look through the stack for the aMethodAST ';
		halt
]

{ #category : 'finalAnalysis' }
ASCodeGenerator >> reifyLocals [
	" for this to work, operations on variables must rename needsContext to needsLocals "

	
]

{ #category : 'finalAnalysis' }
ASCodeGenerator >> removeUnusedBlockClosures [
	"Removal of redundant BlockClosures
After all inlining is completed there will typically be pushes of BlockClosures that are subsequently inlined so that the block itself need never be created. These are turned into pushes of nil."

	basicBlocks do: [ :bb | bb removeUnusedBlockClosures: blockClosures ]
]

{ #category : 'finalAnalysis' }
ASCodeGenerator >> removeUnusedCode [.

]

{ #category : 'API' }
ASCodeGenerator >> returnEmpty [

	" `code` and `currentCodeUnitBlock` are the same"
	^ currentCodeUnit returnEmpty
]

{ #category : 'API' }
ASCodeGenerator >> returnSelf [

	^ currentCodeUnit returnSelfFor: code
]

{ #category : 'API' }
ASCodeGenerator >> returnTop [

	^ currentCodeUnit returnTopFor: code
]

{ #category : 'accessing' }
ASCodeGenerator >> selector [

	^ basicBlocks first selector
]

{ #category : 'accessing' }
ASCodeGenerator >> selector: aSymbol [

	^ code selector: aSymbol
]

{ #category : 'API' }
ASCodeGenerator >> send: aSymbol [

	^ self send: aSymbol targetClass: nil tail: false
]

{ #category : 'API' }
ASCodeGenerator >> send: aSymbol targetClass: aTargetClass [

	^ self send: aSymbol targetClass: aTargetClass tail: false
]

{ #category : 'API' }
ASCodeGenerator >> send: aSymbol targetClass: aTargetClass tail: tail [

	| bb |
	blocksWithCalls add: code.
	tail & tailCallOptimize ifTrue: [
		^ currentCodeUnit
			  sendTail: aSymbol
			  targetClass: aTargetClass
			  for: code ].
	bb := code send: aSymbol targetClass: aTargetClass.
	self addBlock: bb.
	code := bb.
	^ bb
]

{ #category : 'API' }
ASCodeGenerator >> sendEmbedded: aSymbol [

	| bb |
	bb := code embed: aSymbol.
	^ bb ifNotNil: [
		  self addBlock: bb.
		  code := bb ]
]

{ #category : 'accessing' }
ASCodeGenerator >> setBasicBlock: anASCBlock [

	code := anASCBlock
]

{ #category : 'initialization' }
ASCodeGenerator >> setBlockName: aSymbol [ 

	^ code selector: aSymbol class: targetClass generator: self
]

{ #category : 'accessing' }
ASCodeGenerator >> setCodeUnit [

	currentCodeUnit := code
]

{ #category : 'API' }
ASCodeGenerator >> storeVariable: aSymbol [

	^ code storeVariable: aSymbol
]

{ #category : 'settings' }
ASCodeGenerator >> tailCallOptimize: aBoolean [ 
	tailCallOptimize:= aBoolean 
]

{ #category : 'accessing' }
ASCodeGenerator >> targetClass [

^	targetClass
]

{ #category : 'accessing' }
ASCodeGenerator >> targetClass: anObject [ 
targetClass:= anObject 
]

{ #category : 'post processing' }
ASCodeGenerator >> zagBlocks [

	| zagBlocks |
	zagBlocks := OrderedCollection newFrom: basicBlocks.
	zagBlocks addAll: (blockClosures "collect: #codeGenerator").
	^ zagBlocks
]
