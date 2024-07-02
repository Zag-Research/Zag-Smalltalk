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
		'selector',
		'code',
		'basicBlocks',
		'name',
		'localVariables',
		'targetClass',
		'maxDepth',
		'inline',
		'tailCallOptimize',
		'blockClosures',
		'parentCodeGenerator',
		'enclosingScope',
		'implementors',
		'returnBlock',
		'blocksWithCalls'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'as yet unclassified' }
ASCodeGenerator class >> targetClass: aClass ast: anAST [

	^ self new
		  targetClass: aClass;
		  generateFrom: anAST
]

{ #category : 'instance creation' }
ASCodeGenerator class >> targetClass: aClass selector: aString [

	^ self new targetClass: aClass; selector: aString
]

{ #category : 'enumerating' }
ASCodeGenerator >> basicBlocksLinearizeDo: aBlock [

	| rest current |
	current := basicBlocks first.
	rest := basicBlocks allButFirst asSet.
	[ true ] whileNotNil: [
		aBlock value: current.
		current next
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
	[ true ] whileNotNil: [
		aBlock value: current.
		current next ifNotNil: [ :next | current := next ] ifNil: [
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
ASCodeGenerator >> branch: aBasicBlock [

	^ code branch: aBasicBlock
]

{ #category : 'API' }
ASCodeGenerator >> classCase: aCollection selector: aSymbol [

	| case |
	case := code classCase: aCollection selector: aSymbol.
	basicBlocks addAll: case blocks.
	^ aCollection collect: [ :class | case at: class ]
]

{ #category : 'class relationship' }
ASCodeGenerator >> classOf: aString [ 
	self shouldBeImplemented.
]

{ #category : 'API' }
ASCodeGenerator >> compileBlocks: outputter [
	"compiling of blocks probably doesn't work right now"

	(blockClosures reject: #manifest) do: [ :aBlockClosure |
		outputter blockGenerator
			parentCodeGenerator: self;
			generateCode: aBlockClosure block for: BlockClosure ]
]

{ #category : 'compiling' }
ASCodeGenerator >> compiledMethodFor: aSelector for: aClass [ 
	aClass methodDictionary at: aSelector ifPresent: [ ^aClass >> aSelector ].
	aClass superclass ifNil: [ ^nil ].
	^ self compiledMethodFor: aSelector for: aClass superclass 
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

	inline := true
]

{ #category : 'accessing' }
ASCodeGenerator >> enclosingScope: aScope [
	enclosingScope := aScope
]

{ #category : 'evaluating' }
ASCodeGenerator >> eval: aString params: aCollection [

	^ ASCLiteralValue new init: Object new
]

{ #category : 'data flow instructions' }
ASCodeGenerator >> finalAnalysis [

	self inlineWherePossible.
	basicBlocks do: #checkForCalls.
	basicBlocks do: #markReferences.

]

{ #category : 'API' }
ASCodeGenerator >> find: aSymbol [

	^ code at: aSymbol
	"enclosingScope ifNil: [ self halt ].
	^ enclosingScope find: aSymbol"
]

{ #category : 'private - testing' }
ASCodeGenerator >> firstBlock [
	^basicBlocks ifNotEmpty: [ basicBlocks at:1] ifEmpty: [ nil ]
]

{ #category : 'translating' }
ASCodeGenerator >> generateFrom: aMethodOrBlockNode [

	aMethodOrBlockNode zigWalkMethod: self.
	basicBlocks do: [ :aBasicBlock |
		aBasicBlock markBlockClosuresToManifest.
		maxDepth := maxDepth max: aBasicBlock maxDepth ]
]

{ #category : 'as yet unclassified' }
ASCodeGenerator >> globalFind: aString [
	"aString asSymbol isGlobalVariable ifTrue: [ ^ ASStackNamed new name: aString ]"
self halt.
	^ nil
]

{ #category : 'initialization' }
ASCodeGenerator >> implementors: aSelector [

	"^ SystemNavigation default allImplementorsOf: aSymbol"
	(implementors includesKey: aSelector) ifTrue: [ ^implementors at: aSelector ] ifFalse: [ ^ {} ]
]

{ #category : 'accessing' }
ASCodeGenerator >> inferType: anExpression [

	^ nil
	"anExpression ifNil: [ ^ nil ].
	^ anExpression inferType: self"
	"anExpression isASSend ifTrue: [ ^ self inferType: anExpression ]."
]

{ #category : 'initialization' }
ASCodeGenerator >> initCode [

	code := ASCMethodBlock new
]

{ #category : 'initialization' }
ASCodeGenerator >> initialize [

	super initialize.
	self initCode.
	basicBlocks := OrderedCollection new.
	basicBlocks add: code.
	blockClosures := OrderedCollection new.
	blocksWithCalls := Set new.
	maxDepth := 0.
	inline := false.
	tailCallOptimize := true.

	implementors := Dictionary new.
	implementors at: #not put: {
			(True >> #not).
			(False >> #not) }.
	implementors at: #ifTrue: put: {
			(True >> #ifTrue:).
			(False >> #ifTrue:) }.
	implementors at: #ifFalse: put: {
			(True >> #ifFalse:).
			(False >> #ifFalse:) }.
	implementors at: #ifTrue:ifFalse: put: {
			(True >> #ifTrue:ifFalse:).
			(False >> #ifTrue:ifFalse:) }.
	implementors at: #ifFalse:ifTrue: put: {
			(True >> #ifFalse:ifTrue:).
			(False >> #ifFalse:ifTrue:) }.
	implementors at: #isLarge put: { (SmallInteger >> #isLarge) }.
	implementors at: #not put: { (Number >> #abs) }.
	implementors at: #value put: { (ASTBlockClosure >> #value) }.
	implementors at: #whileTrue put: { (ASTBlockClosure >> #whileTrue) }.
	implementors
		at: #fibonacci_fast
		put: { (Integer >> #fibonacci_fast) }.
	implementors
		at: #fibonacci_accumulator:prev:
		put: { (Integer >> #fibonacci_accumulator:prev:) }
]

{ #category : 'translating' }
ASCodeGenerator >> inlineBlock: aSymbol next: common [

	| inlineBlock savedReturn savedTargetClass |
	savedTargetClass := targetClass.
	inlineBlock := self newInlineBlockClosure.
	savedReturn := returnBlock.
	[
	returnBlock := common.
	self branch: inlineBlock.
	code := inlineBlock.
	(code stackBelow: aSymbol numArgs) value block zigWalkInlineMethod:
		self.
	code := returnBlock ] ensure: [
		returnBlock := savedReturn.
		targetClass := savedTargetClass ]
]

{ #category : 'as yet unclassified' }
ASCodeGenerator >> inlineBlock: aString tail: aBoolean [
	"do we need to test for recursion?"
	
	"find the block on the stack"
	code stack value block zigWalkInlineMethod: self
]

{ #category : 'API' }
ASCodeGenerator >> inlineMethod: aCompileMethod [

	^ self
		  inlineMethod: aCompileMethod
		  for: aCompileMethod classBinding value
]

{ #category : 'accessing' }
ASCodeGenerator >> inlineMethod: aCompileMethod for: inlinedTargetClass [

	| inlineBlock savedReturn savedTargetClass |
	(code isRecursive: aCompileMethod) ifTrue: [ ^ self ].
	savedTargetClass := targetClass.
	"create a new block "
	inlineBlock := self newInlineMethod.
	savedReturn := returnBlock.
	[
	targetClass := inlinedTargetClass.
	returnBlock := self newInline.
	self branch: inlineBlock.
	code := inlineBlock.
	(ASPharoCompiler new astFor: aCompileMethod) zigWalkInlineMethod:
		self ] ensure: [ "when you hit a return while walking the method you need to not do returnTop you need to return to returnBlock""self can hold the return block"
		code := returnBlock.
		returnBlock := savedReturn.
		targetClass := savedTargetClass ]
]

{ #category : 'data flow instructions' }
ASCodeGenerator >> inlineWherePossible [

	| temp |
	[ blocksWithCalls isEmpty ] whileFalse: [
		temp := blocksWithCalls.
		blocksWithCalls := Set new.
		temp do: [ :block |
			block inlineIfPossible: self ] ]
]

{ #category : 'initialization' }
ASCodeGenerator >> inlining: aBoolean [
	inline := aBoolean
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
ASCodeGenerator >> locals: locals indirectParameters: parameters [

	code locals: locals parameters: parameters
]

{ #category : 'accessing' }
ASCodeGenerator >> locals: locals parameters: arguments [

	code initSelf: targetClass.
	code locals: locals parameters: arguments
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

	^ basicBlocks add: (ASCInlineBlock new)
]

{ #category : 'private - testing' }
ASCodeGenerator >> newInlineBlockClosure [

	^ basicBlocks add: ASCInlinedClosureBlock new
]

{ #category : 'private - testing' }
ASCodeGenerator >> newInlineMethod [

	^ basicBlocks add: ASCInlinedMethodBlock new
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
	block := ASCBlockClosureValue new. "with: anASBlock"
	"block name: name asSymbol."
	blockClosures add: block.
	code setStackForBlock: block.
	block asBlock: anASBlock.
	block parent: self.
	code addOperation: (ASCBlock with: block)
]

{ #category : 'API' }
ASCodeGenerator >> pushLiteral: aLiteral [

	code addOperation: (ASCLiteral with: aLiteral)
]

{ #category : 'API' }
ASCodeGenerator >> pushSelf [

	^ self pushVariable: #self
]

{ #category : 'API' }
ASCodeGenerator >> pushVariable: variable [

	^ code pushVariable: variable
]

{ #category : 'as yet unclassified' }
ASCodeGenerator >> requireContext [

	code getContext
]

{ #category : 'API' }
ASCodeGenerator >> returnSelf [

	code returnSelf
]

{ #category : 'API' }
ASCodeGenerator >> returnTop [

	code
		returnTop
]

{ #category : 'API' }
ASCodeGenerator >> returnTopInline [

	code popAndCopyTopToSelf.
	self branch: returnBlock
]

{ #category : 'accessing' }
ASCodeGenerator >> scopeFind: aSymbol [
	enclosingScope ifNil: [ ^self globalFind: aSymbol ].
	^ enclosingScope find: aSymbol 
]

{ #category : 'accessing' }
ASCodeGenerator >> selector [

	^	selector
]

{ #category : 'accessing' }
ASCodeGenerator >> selector: aSymbol [

	selector := aSymbol.
	code selector: aSymbol 
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
	aTargetClass ifNotNil: [ "check if this is an embeddable send"
		aTargetClass isBlock ifTrue: [
			^ self inlineBlock: aSymbol next: nil ].
		(ASCEmbed isEmbedded:
			 (self compiledMethodFor: aSymbol for: aTargetClass)) ifTrue: [
			^ self sendEmbedded: aSymbol ].
		"check if this is a self call"
		(self sendInline: aSymbol targetClass: aTargetClass tail: tail)
			ifTrue: [ ^ true "return something?" ] ].
	blocksWithCalls add: code.
	bb := code
		      send: aSymbol
		      targetClass: aTargetClass
		      tail: tail & tailCallOptimize.
	^ bb ifNotNil: [
		  basicBlocks add: bb.
		  code := bb ]
]

{ #category : 'API' }
ASCodeGenerator >> sendEmbedded: aSymbol [

	| bb |
	bb := code embed: aSymbol.
	^ bb ifNotNil: [
		  basicBlocks add: bb.
		  code := bb ]
]

{ #category : 'API' }
ASCodeGenerator >> sendInline: aSymbol targetClass: aTargetClass tail: tail [

	| symbolImplementors common targets |
	"check if we're recursive"
	inline ifFalse: [ ^ false ].

	"check if this is a self call"
	targetClass == aTargetClass ifTrue: [
		self inlineMethod: aTargetClass >> aSymbol.
		^ true ].
	"extract system nav and for now base if off a list"
	symbolImplementors := self implementors: aSymbol.
	"move to 'ensure' to remove last"
	(symbolImplementors size between: 1 and: 4) ifFalse: [ ^ false ].
	common := self newInline.
	tail ifTrue: [ common returnTop ].
	ASTBlockClosure == aTargetClass ifTrue: [
		^ self inlineBlock: aSymbol next: common ].
	[
	targets := self
		           classCase: (symbolImplementors collect: #methodClass)
		           selector: aSymbol.
	targets with: symbolImplementors do: [ :target :compileMethod |
		self setBasicBlock: target.
		"walk the sends method AST"
		self inlineMethod: compileMethod for: target.
		self branch: common ] ] ensure: [ self setBasicBlock: common ].
	^ true
]

{ #category : 'accessing' }
ASCodeGenerator >> setBasicBlock: anASCBlock [

	code := anASCBlock
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

{ #category : 'accessing' }
ASCodeGenerator >> topOfStackType [

	^ self inferType: (code at: 0) value
]

{ #category : 'post processing' }
ASCodeGenerator >> zagBlocks [

	| zagBlocks |
	zagBlocks := OrderedCollection newFrom: basicBlocks.
	zagBlocks addAll:
		(blockClosures collect: [ :bc | bc codeGenerator zagBlocks]).
	^ zagBlocks
]
