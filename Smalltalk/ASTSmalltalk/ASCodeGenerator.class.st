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
		'enclosingScope'
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
	^ code find: aSymbol 
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
	self tryInlining.
	basicBlocks do: [ :aBasicBlock |
		aBasicBlock markBlockClosuresToManifest.
		maxDepth := maxDepth max: aBasicBlock maxDepth ].
	blockClosures := blockClosures select: #manifest.
	blockClosures do: [ :aBlockClosure |
		self compileBlock: aBlockClosure ]
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
	tailCallOptimize := true
]

{ #category : 'accessing' }
ASCodeGenerator >> inline: anASCSend [

	| ast |
	targetClass == anASCSend target ifFalse: [ ^ false ].

	self halt. "inline a self method"
	ast := ASPharoCompiler new astFor:
		       anASCSend target >> anASCSend selector.
	output generateCode: ast for: anASCSend target.
	"look at pending components, expects a codeGenerator so need to create one and add to components"
	^ false
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
	bb := code embed: aSymbol.
	^ bb ifNotNil: [
		  basicBlocks add: bb.
		  code := bb ]
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
ASCodeGenerator >> tryInlining [
	inline ifFalse: [ ^ self ].
	[ | bbCopy | 
		bbCopy := OrderedCollection new.
		bbCopy addAll: basicBlocks.
	basicBlocks anySatisfy: [ :aBasicBlock |
		aBasicBlock canBeInlinedBy: self. ] ] whileTrue
]

{ #category : 'post processing' }
ASCodeGenerator >> zagBlocks [

	| zagBlocks |
	zagBlocks := OrderedCollection newFrom: basicBlocks.
	zagBlocks addAll: (blockClosures collect: [ :bc | InspectBlock from: bc ]).
	^ zagBlocks
]
