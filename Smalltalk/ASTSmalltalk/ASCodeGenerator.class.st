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
		'currentCodeUnit'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'as yet unclassified' }
ASCodeGenerator class >> targetClass: aClass selector: aSymbol [

	^ self new
		  targetClass: aClass;
		  generateFrom: (ASPharoCompiler new astFor: (ASInliner lookup: aSymbol class: aClass) method)
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

{ #category : 'API' }
ASCodeGenerator >> branch: aBasicBlock [

	^ aBasicBlock ifNotNil: [ code branch: aBasicBlock ]
]

{ #category : 'class relationship' }
ASCodeGenerator >> classOf: aString [ 
	self shouldBeImplemented.
]

{ #category : 'API' }
ASCodeGenerator >> compileBlocks: outputter [
	"compiling of blocks probably doesn't work right now"

	(blockClosures select: #manifest) do: [ :aBlockClosure |
		outputter blockGenerator
			parentCodeGenerator: self;
			generateCode: aBlockClosure block for: BlockClosure ]
]

{ #category : 'finalAnalysis' }
ASCodeGenerator >> compileRemainingBlockClosures [

	blockClosures do: #error
]

{ #category : 'finalAnalysis' }
ASCodeGenerator >> createContextAndStackOffsets [
]

{ #category : 'accessing' }
ASCodeGenerator >> deriveOwnership [
	"drive the ownership of compilable unit blocks"

	| varsModified varsReferenced |
	varsModified := Dictionary new.
	varsReferenced  := Dictionary new.

	"get each block to gather variable usage"
	basicBlocks do: [ :bb | bb deriveVariableReferences: varsReferenced modifies: varsModified ].
	"
	1. If a variable is only referenced in the method, it will be put in the Context (or just on the stack if no context is created).
   2. If the variable is only referenced in one BlockClosure then it will be created as a local variable there.
   3. For values referenced in two or more places, modified in at least one, the default would be to put them in the Context. However, if a BlockClosure has a reference to the Context and the closure gets moved to the heap, it will drag the entire stack with it. Therefore the only closures that reference the context will be ones with non-local returns (or that create closures that need a context reference). Variables referenced in non-local-return closures will be placed in the context.
   4. All other variables will be placed in a closure that modifies the variable.
"
self halt.
	varsReferenced keysAndValuesDo: [ :k :v |
		(varsModified includes: k) ifFalse: [ 
		"if there's only one usage of a variable then no issues"
		v size = 1 ifTrue: [ v keys first owns add: k ].
		"there are more then one we need to know modifies but don't"
		]
	].
	varsModified keysAndValuesDo: [ :k :v |
		"if there's only one usage of a variable then no issues"
		v size = 1 ifTrue: [ v keys first owns add: k ].
		"there are more then one we need to know modifies but don't"
	]
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
	self createContextAndStackOffsets.
]

{ #category : 'private - testing' }
ASCodeGenerator >> firstBlock [
	^basicBlocks ifNotEmpty: [ basicBlocks at:1] ifEmpty: [ nil ]
]

{ #category : 'data flow instructions' }
ASCodeGenerator >> flowAnalysis [

	basicBlocks do: #becomeJoinBlock.
	basicBlocks do: #markReferences
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
	blockClosures := OrderedCollection new.
	blocksWithCalls := Set new.
	inline := 0.
	tailCallOptimize := true
]

{ #category : 'accessing' }
ASCodeGenerator >> inline: aBooleanOrInteger [

	inline := aBooleanOrInteger isInteger
		          ifTrue: [ aBooleanOrInteger ]
		          ifFalse: [ 1000 ]
]

{ #category : 'translating' }
ASCodeGenerator >> inlineBlock: offset next: common [

	| savedReturn savedTargetClass |
	savedTargetClass := targetClass.
	savedReturn := returnBlock.
	[
	| inlineBlock |
	returnBlock := common.
	inlineBlock := self newInlineBlockClosure.
	self branch: inlineBlock.
	code := inlineBlock.
	self halt.
	(code stackBelow: offset) stackValue block zigWalkMethod: self.
	code := returnBlock ] ensure: [
		returnBlock := savedReturn.
		targetClass := savedTargetClass ]
]

{ #category : 'finalAnalysis' }
ASCodeGenerator >> inlineDependentPrimitives [
]

{ #category : 'accessing' }
ASCodeGenerator >> inlineMethod: aMethodAST for: inlinedTargetClass from: anASCSend [

	| inlineBlock savedTargetClass |
	(self recursiveBasicBlock: aMethodAST) ifNotNil: [ :block |
		^ self
			  inlineRecursiveMethod: aMethodAST
			  block: block
			  from: anASCSend ].
	self halt.
	savedTargetClass := targetClass.
	inlineBlock := self newInlineMethod: aMethodAST.
	inlineBlock copyStackFrom: code.
	[
	targetClass := inlinedTargetClass.
	returnBlock := self getOrMakeReturnBlock.
	code removeLastOperation.
	self branch: inlineBlock.
	code := inlineBlock.
	(ASPharoCompiler new astFor: aMethodAST) zigWalkMethod: self ]
		ensure: [ "when you hit a return while walking the method you need to not do returnTop you need to return to returnBlock""self can hold the return block"
			code := returnBlock.
			targetClass := savedTargetClass ]
]

{ #category : 'inlining' }
ASCodeGenerator >> inlineMultiple: methods from: anASCSend exhaustive: isExhaustive [

	| case joinBlock |
	isExhaustive ifTrue: [
		code removeLastOperation.
		joinBlock := ASCJoinBlock new branch: anASCSend next becomeJoinBlock.
		basicBlocks add: joinBlock ].
	case := code
		        classCase: (methods collect: #methodClass)
		        selector: methods first selector.
	isExhaustive ifFalse: [
		code swapLastOperations.
		joinBlock := anASCSend next ].
	^ methods do: [ :methodAST |
		  | targetBlock |
		  targetBlock := case at: methodAST methodClass.
		  targetBlock next: joinBlock.
		  basicBlocks add: targetBlock.
		  self setBasicBlock: targetBlock.
		  "walk the sends method AST"
		  self inlineMethod: methodAST for: targetBlock from: anASCSend ]
]

{ #category : 'inlining' }
ASCodeGenerator >> inlinePrimitive: aCompiledMethod for: aClass from: anASCSend [

	| primitive |
	primitive := aCompiledMethod primitive.
	returnBlock := self getOrMakeReturnBlock.
	self halt.
	(primitive between: 201 and: 205) ifTrue: [ ^ self inlineValue: primitive for: aClass next: returnBlock ].
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
ASCodeGenerator >> inlineValue: anInteger for: aClass next: aBasicBlock [

	aClass == BlockClosure ifTrue: [ ^ self inlineBlock: anInteger - 201 next: aBasicBlock ].
	self halt " handle Symbol value: "
]

{ #category : 'finalAnalysis' }
ASCodeGenerator >> inlineWherePossible [

	| temp |
	[
	inline > 0 ifFalse: [ ^ self ].
	inline := inline - 1.
	blocksWithCalls isEmpty ] whileFalse: [
		temp := blocksWithCalls.
		blocksWithCalls := Set new.
		temp do: [ :block |
			code := block.
			block inlineIfPossible: self ] ].
	basicBlocks do: [ : block | block inlineWithFixedCFG: self ]
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

{ #category : 'accessing' }
ASCodeGenerator >> name [
	^ name
]

{ #category : 'private - testing' }
ASCodeGenerator >> newInline [

	^ basicBlocks add: ASCJoinBlock new
]

{ #category : 'private - testing' }
ASCodeGenerator >> newInlineBlockClosure [

	^ basicBlocks add: currentCodeUnit blockClosureClass
]

{ #category : 'instance creation' }
ASCodeGenerator >> newInlineMethod: aMethodAST [

	| newBlock |
	newBlock := ASCInlineMethodBlock new .
	^ basicBlocks add: newBlock
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
ASCodeGenerator >> pushBlock: anASBlock [

	| block |
	block := AStBlock new block: anASBlock.
	blockClosures add: block.
	code addOperation: (ASCBlock with: block)
]

{ #category : 'API' }
ASCodeGenerator >> pushLiteral: aLiteral [

	^ code pushLiteral: aLiteral
]

{ #category : 'as yet unclassified' }
ASCodeGenerator >> pushNil [

	^ code pushNil
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
ASCodeGenerator >> removeUnusedBlockClosures [
]

{ #category : 'finalAnalysis' }
ASCodeGenerator >> removeUnusedCode [.

]

{ #category : 'API' }
ASCodeGenerator >> returnEmpty [

	" `code` and `currentCodeUnitBlock` are the same"
	^ currentCodeUnit returnEmpty
]

{ #category : 'examples' }
ASCodeGenerator >> returnLiteral: literal [

	self pushLiteral: literal.
	self returnTop.
	^ true
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
	basicBlocks add: bb.
	code := bb.
	^ bb
]

{ #category : 'API' }
ASCodeGenerator >> sendEmbedded: aSymbol [

	| bb |
	bb := code embed: aSymbol.
	^ bb ifNotNil: [
		  basicBlocks add: bb.
		  code := bb ]
]

{ #category : 'accessing' }
ASCodeGenerator >> setBasicBlock: anASCBlock [

	code := anASCBlock
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
