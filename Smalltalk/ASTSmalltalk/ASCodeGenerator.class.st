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
		'output',
		'tailCallOptimize',
		'blockClosures',
		'parentCodeGenerator',
		'enclosingScope',
		'embeddings',
		'implementors',
		'returnBlock',
		'embeddingReturnTypes'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'enumerating' }
ASCodeGenerator >> basicBlocksLinearizeDo: aBlock [

	| rest current |
	ASCBlock resetLabelIndex.
	current := basicBlocks first.
	rest := basicBlocks allButFirst asSet.
	[ true ] whileNotNil: [
		aBlock value: current.
		current next
			ifNil: [
				rest isEmpty ifTrue: [ ^ self ].
				current := rest atRandom ifNil: [ ^ self ] ]
			ifNotNil: [ :next | current := next ].
		rest remove: current ]
]

{ #category : 'enumerating' }
ASCodeGenerator >> basicBlocksUnorderedDo: aBlock [ 
	basicBlocks do: aBlock
]

{ #category : 'API' }
ASCodeGenerator >> branch: aBasicBlock [

	code branch: aBasicBlock.
	aBasicBlock addSource: code
]

{ #category : 'API' }
ASCodeGenerator >> classCase: aCollection [

	| case |
	case := code classCase: aCollection.
	basicBlocks addAll: case blocks.
	^ aCollection collect: [ :class | case at: class ]
]

{ #category : 'class relationship' }
ASCodeGenerator >> classOf: aString [ 
	self shouldBeImplemented.
]

{ #category : 'API' }
ASCodeGenerator >> compileBlock: aBlockClosure [
	"compiling of blocks doesn't work right now"

	aBlockClosure manifest ifFalse: [ ^self].
		code pushBlockNamed: aBlockClosure.
		output compileBlock: aBlockClosure
]

{ #category : 'compiling' }
ASCodeGenerator >> compileMethodFor: aSelector for: aClass [ 
	aClass methodDictionary at: aSelector ifPresent: [ ^aClass >> aSelector ].
	aClass superclass ifNil: [ ^nil ].
	^ self compileMethodFor: aSelector for: aClass superclass 
]

{ #category : 'API' }
ASCodeGenerator >> drop [

	code
		doDrop;
		putSimple: #drop
]

{ #category : 'API' }
ASCodeGenerator >> dup [

	code
		doDup;
		putSimple: #dup
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

{ #category : 'post processing' }
ASCodeGenerator >> finalAnalysis [


]

{ #category : 'API' }
ASCodeGenerator >> find: aSymbol [
	^ code find: aSymbol 
	"enclosingScope ifNil: [ self halt ].
	^ enclosingScope find: aSymbol"
]

{ #category : 'private - testing' }
ASCodeGenerator >> firstBlock [
	^basicBlocks ifNotEmpty: [ basicBlocks at:1] ifEmpty: [ nil ]
]

{ #category : 'translating' }
ASCodeGenerator >> generate: aMethodOrBlockNode [

	aMethodOrBlockNode zigWalkMethod: self.
	basicBlocks do: [ :aBasicBlock |
		aBasicBlock markBlockClosuresToManifest.
		maxDepth := maxDepth max: aBasicBlock maxDepth ].
	blockClosures := blockClosures select: #manifest.
	blockClosures do: [ :aBlockClosure |
		self compileBlock: aBlockClosure ]
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
	^implementors at: aSelector
]

{ #category : 'accessing' }
ASCodeGenerator >> inferType: anExpression [
	anExpression ifNil: [ ^ nil ].
	^ anExpression inferType: self
	"anExpression isASSend ifTrue: [ ^ self inferType: anExpression ]."
]

{ #category : 'initialization' }
ASCodeGenerator >> initialize [

	super initialize.
	code := ASCMethodBlock new.
	basicBlocks := OrderedCollection new.
	basicBlocks add: code.
	blockClosures := OrderedCollection new.
	maxDepth := 0.
	inline := false.
	tailCallOptimize := true.
	embeddings := {
		              (Integer >> #-).
		              (Integer >> #'<=').
		              (SmallInteger >> #<).
		              (SmallInteger >> #+) }.
	embeddingReturnTypes := Dictionary new.
	embeddingReturnTypes at: #- put: SmallInteger.
	embeddingReturnTypes at: #'<=' put: Boolean.
	embeddingReturnTypes at: #< put: Boolean.
	embeddingReturnTypes at: #+ put: SmallInteger.

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
	implementors at: #whileTrue put: { (ASTBlockClosure >> #whileTrue) }
]

{ #category : 'translating' }
ASCodeGenerator >> inlineBlock: aString next: common [

	| inlineBlock savedReturn savedTargetClass |
	savedTargetClass := targetClass.
	inlineBlock := self newInline.
	savedReturn := returnBlock.
	[
		returnBlock := self newInline.
		self branch: inlineBlock.
		code := inlineBlock.
		code stack top block zigWalkInlineMethod: self.
		code := returnBlock ] ensure: [
			returnBlock := savedReturn.
			targetClass := savedTargetClass ]
]

{ #category : 'as yet unclassified' }
ASCodeGenerator >> inlineBlock: aString tail: aBoolean [ 
"do we need to test for recursion?"
	code stack value block zigWalkInlineMethod: self

	
]

{ #category : 'accessing' }
ASCodeGenerator >> inlineMethod: aCompileMethod [

	| inlineBlock savedReturn savedTargetClass |
	self halt.
	(code isRecursive: aCompileMethod) ifTrue: [ ^false ].
	savedTargetClass := targetClass.
	"create a new block "
	inlineBlock := self newInline.
	savedReturn := returnBlock.
	[
	targetClass := aCompileMethod classBinding value.
	returnBlock := self newInline.
	self branch: inlineBlock.
	code := inlineBlock.
	(ASPharoCompiler new astFor: aCompileMethod) zigWalkInlineMethod:
		self
	 ] ensure: [ "when you hit a return while walking the method you need to not do returnTop you need to return to returnBlock""self can hold the return block"
		code := returnBlock.
		returnBlock := savedReturn.
		targetClass := savedTargetClass ]
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

{ #category : 'API' }
ASCodeGenerator >> isEmbedded: aCompileMethod [
 	"better to just check if this method contains a primitive and not in the list"
	^ embeddings hasLiteral: aCompileMethod 
]

{ #category : 'accessing' }
ASCodeGenerator >> locals: locals indirectArguments: arguments [

	"output forClass ifNotNil: [ :theClass |
		code doPush: (ASCClassValue new init: theClass) ].
	code pushSelfNamed: self selfName."
	code locals: locals arguments: arguments
]

{ #category : 'accessing' }
ASCodeGenerator >> locals: locals indirectParameters: parameters [
	"output forClass ifNotNil: [ :theClass |
		code doPush: (ASCClassValue new init: theClass) ].
	code pushSelfNamed: self selfName."
	code locals: locals arguments: parameters
]

{ #category : 'accessing' }
ASCodeGenerator >> locals: locals parameters: arguments [

	code pushSelf: targetClass.
	code locals: locals parameters: arguments
]

{ #category : 'compiling' }
ASCodeGenerator >> lookupLocal: aSymbol [

	^	localVariables detect: [ :variable |
		aSymbol = variable name ]
]

{ #category : 'compiling' }
ASCodeGenerator >> lookupName: aString block: anUndefinedObject [
	"self shouldBeImplemented."

	^code find: aString
]

{ #category : 'accessing' }
ASCodeGenerator >> lookupObject: anObject [

	^ output lookupObject: anObject
]

{ #category : 'API' }
ASCodeGenerator >> makeContextRequired [

	^ code makeContextRequired
]

{ #category : 'serializing' }
ASCodeGenerator >> manifest: anASCBlock [

	code makeContextRequired
]

{ #category : 'accessing' }
ASCodeGenerator >> name [
	^ name
]

{ #category : 'private - testing' }
ASCodeGenerator >> newInline [

	^ basicBlocks add: (ASCInlineBlock for: code)
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
ASCodeGenerator >> output: anASFileContext [

	output := anASFileContext
]

{ #category : 'accessing' }
ASCodeGenerator >> parentCodeGenerator: aCodeGenerator [

	aCodeGenerator ifNotNil: [
		parentCodeGenerator := aCodeGenerator ]
]

{ #category : 'API' }
ASCodeGenerator >> popVariable: aSymbol [

	^ code
		modifies: aSymbol;
		popVariable: aSymbol
]

{ #category : 'API' }
ASCodeGenerator >> primitive: primitiveNumber [

	code primitive: primitiveNumber
]

{ #category : 'API' }
ASCodeGenerator >> pushArray: arraySize [

	^ code doPush: (ASCArray with: arraySize )

	
]

{ #category : 'API' }
ASCodeGenerator >> pushBlock: anASBlock [

	| block |
	block := ASCBlock with: anASBlock.
	"block name: name asSymbol."
	blockClosures add: block.
	code
		doPush: block;
		put: block
]

{ #category : 'API' }
ASCodeGenerator >> pushLiteral: aLiteral [

	code
		doPush: (ASCLiteralValue new init: aLiteral);
		put: (ASCLiteral with: aLiteral)
]

{ #category : 'API' }
ASCodeGenerator >> pushSelf [

	code put: (ASCPushVariable with: (code at: #self))
]

{ #category : 'API' }
ASCodeGenerator >> pushVariable: variable [

	^ code pushVariable: variable
]

{ #category : 'API' }
ASCodeGenerator >> putEmbedded: aSymbol [

	code put: aSymbol
]

{ #category : 'data flow instructions' }
ASCodeGenerator >> resolveFlowDependencies [

	| blocksToProcess |
	blocksToProcess := basicBlocks.
	[ blocksToProcess isEmpty ] whileFalse: [
		| temp |
		temp := blocksToProcess.
		blocksToProcess := Set new.
		temp do: [ :block |
			blocksToProcess addAll: block updateDependencies ] ]
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
		self halt.
		aTargetClass isClosure ifTrue: [ 
			"sepectial case of inlining a block closure"	
			(self inlineBlock: aSymbol tail: tail) ifTrue: [
			^ true "return something?" ]
		].
		(self isEmbedded: (self compileMethodFor: aSymbol for: aTargetClass)) ifTrue: [
			^ self sendEmbedded: aSymbol ].
		"check if this is a self call"
		(self sendInline: aSymbol targetClass: aTargetClass tail:tail) ifTrue: [
			^ true "return something?" ] ].
	self manifest: code.
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
	bb := code embed: aSymbol return: (embeddingReturnTypes at: aSymbol).
	^ bb ifNotNil: [
		  basicBlocks add: bb.
		  code := bb ]
]

{ #category : 'API' }
ASCodeGenerator >> sendInline: aSelector targetClass: aTargetClass tail: tail [
	"return false if inline flag isn't set"

	inline ifFalse: [ ^ false ].


	"check if this is a self call"
	targetClass == aTargetClass
		ifTrue: [ "we know the target class and the current class being compiled are not the same""rename stack elements to be named values""self/super/literal don't need a send""stack clone self setBasicBlock""build ast""walk the sends method AST""lets check number of implmentors""Daniel just use a short list of compile methods for now"
			^ self inlineMethod: aTargetClass >> aSelector ]
		ifFalse: [
			| symbolImplementors common targets |
			"find the classes that implement the given selector"
			symbolImplementors := self implementors: aSelector.
			"move to 'ensure' to remove last"
			symbolImplementors size < 10 ifFalse: [ ^ false ].
			common := self newInline.
			tail ifTrue: [ common returnTop ].
			[
			targets := self classCase:
				           (symbolImplementors collect: [ :compileMethod |
					            compileMethod methodClass ]).
			targets with: symbolImplementors do: [ :target :compileMethod |
				self setBasicBlock: target.
				"walk the sends method AST"
				self inlineMethod: compileMethod.
				self branch: common ] ] ensure: [ self setBasicBlock: common ] ].
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

{ #category : 'as yet unclassified' }
ASCodeGenerator >> topOfStackType [

	^ self inferType: code stack value
]

{ #category : 'post processing' }
ASCodeGenerator >> zagBlocks [

	| zagBlocks |
	zagBlocks := OrderedCollection newFrom: basicBlocks.
	zagBlocks addAll: (blockClosures collect: [ :bc | InspectBlock from: bc ]).
	^ zagBlocks
]
