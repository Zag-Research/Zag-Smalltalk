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
		'inlineSelectors'
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
	enclosingScope ifNil: [ self halt ]. 
	^ enclosingScope find: aSymbol 
]

{ #category : 'API' }
ASCodeGenerator >> findTargetClass: aTarget [

^	aTarget = #self
		ifTrue: [ targetClass ]
		ifFalse: [
			aTarget = #super
				ifTrue: [ targetClass superclass ]
				ifFalse: [ nil ] ]
]

{ #category : 'accessing' }
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

{ #category : 'accessing' }
ASCodeGenerator >> inferType: anExpression [
	anExpression isASSelf ifTrue: [ ^ targetClass ].
	anExpression isASLiteral ifTrue: [ ^ anExpression literal class ].
	^ nil
	"anExpression isASSend ifTrue: [ ^ self inferType: anExpression ]."
]

{ #category : 'initialization' }
ASCodeGenerator >> initialize [

	super initialize.
	code := ASCMethodBlock new.
	code nonlocalNameLookupObject: self.
	basicBlocks := OrderedCollection new.
	basicBlocks add: code.
	blockClosures := OrderedCollection new.
	maxDepth := 0.
	inline := false.
	tailCallOptimize := true.
	inlineSelectors := OrderedCollection new.
	embeddings := {
		              (Integer >> #-).
		              (Integer >> #'<=') }
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
ASCodeGenerator >> locals: locals parameters: arguments [

	output forClass ifNotNil: [ :theClass |
		code doPush: (ASCClassValue new init: theClass) ].
	code pushSelfNamed: self selfName.
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

{ #category : 'serializing' }
ASCodeGenerator >> manifest: anASCBlock [

	code makeContextRequired
]

{ #category : 'accessing' }
ASCodeGenerator >> name [
	^ name
]

{ #category : 'instance creation' }
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
aCodeGenerator ifNotNil: [ 	parentCodeGenerator := aCodeGenerator.
	code nonlocalNameLookupObject: aCodeGenerator]
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

	| value |
	value := ASCClassValue new init: targetClass.
	code
		needs: #self;
		doPush: value;
		put: (ASCPushVariable with: (code find: #self) top)
]

{ #category : 'API' }
ASCodeGenerator >> pushVariable: variable [

	^ code
		needs: variable;
		pushVariable: variable
]

{ #category : 'API' }
ASCodeGenerator >> putEmbedded: aSymbol [

	code put: aSymbol
]

{ #category : 'as yet unclassified' }
ASCodeGenerator >> returnCode [ 
	^ code.
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

{ #category : 'accessing' }
ASCodeGenerator >> selector [

	^	selector
]

{ #category : 'accessing' }
ASCodeGenerator >> selector: aSymbol [

	selector := aSymbol.
	"label for blocks of code"
	name := output name zigName.
	code selector: aSymbol 
]

{ #category : 'as yet unclassified' }
ASCodeGenerator >> selfName [
	^ #self
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
		(self isEmbedded: aTargetClass >> aSymbol) ifTrue: [
			^ self sendEmbedded: aSymbol ].
		"check if this is a self call"
		inline & (self sendInline: aSymbol targetClass: aTargetClass) ifTrue: [
			^ true"return something?" ].
		].
	self manifest: code.
	bb := code
		      send: aSymbol
		      targetClass: aTargetClass
		      tail: tail & tailCallOptimize.
	^bb ifNotNil: [
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
ASCodeGenerator >> sendInline: aSymbol targetClass: aTargetClass [
	"check if we're recursive"
	(inlineSelectors contains: aSymbol) ifTrue: [ ^false ].
	inlineSelectors addLast: aSymbol.
	
	"check if this is a self call"
	targetClass == aTargetClass ifTrue: [ "we know the target class and the current class being compiled are not the same" "rename stack elements to be named values""self/super/literal don't need a send""stack clone self setBasicBlock""build ast"
		
		"walk the sends method AST"
		(ASPharoCompiler new astFor: aTargetClass >> aSymbol)
		          zigWalkMethod: self
		 ]
	"lets check number of implmentors"
	"Daniel just use a short list of compile methods for now"
	ifFalse: [ | implementors common targets |
		"extract system nav and for now base if off a list"
		implementors := SystemNavigation default allImplementorsOf: aSymbol.
		"move to 'ensure' to remove last"
		implementors size < 10 ifFalse: [ inlineSelectors removeLast. ^ false  ].
		
		common := self newInline.
		targets := self classCase: (implementors collect: [ :compileMethod | compileMethod methodClass]).
		targets with: implementors do: [ :target :compileMethod |
			self setBasicBlock: target.
			
			"walk the sends method AST"
			(ASPharoCompiler new astFor: compileMethod)
		          zigWalkInline: self.
			self branch: common
		].
		self setBasicBlock: common
		"result isReturn ifTrue: [ ^ result expression zigWalk: self ]."
	].
 	inlineSelectors removeLast.
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

{ #category : 'post processing' }
ASCodeGenerator >> zagBlocks [

	| zagBlocks |
	zagBlocks := OrderedCollection newFrom: basicBlocks.
	zagBlocks addAll: (blockClosures collect: [ :bc | InspectBlock from: bc ]).
	^ zagBlocks
]
