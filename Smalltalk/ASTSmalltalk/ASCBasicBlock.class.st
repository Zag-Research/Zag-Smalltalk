"
I represent a Basic Block for ASCodeGenerator.

Every Method or block is made up of a graph of these.

I have a `then` field which is the block I will go to if the named test is true.

If there is no test, or if the test is false, then I will continue with the `else` block.
"
Class {
	#name : 'ASCBasicBlock',
	#superclass : 'Object',
	#instVars : [
		'stack',
		'maxDepth',
		'operations',
		'label',
		'method',
		'hash',
		'stackDepth',
		'context',
		'sources'
	],
	#classVars : [
		'SequenceNumber'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'instance creation' }
ASCBasicBlock class >> for: aBasicBlock [

	^ super new method: aBasicBlock namedMethod
]

{ #category : 'class initialization' }
ASCBasicBlock class >> initialize [

	SequenceNumber := 0
]

{ #category : 'comparing' }
ASCBasicBlock >> = other [

	^ hash = other hash and: [ other class = self class ]
]

{ #category : 'accessing' }
ASCBasicBlock >> at: nameOrOffset [

	^ stack at: nameOrOffset
]

{ #category : 'accessing' }
ASCBasicBlock >> blocks [

	| blocks block |
	blocks := OrderedCollection new.
	block := self next.
	[ block notNil ] whileTrue: [
		blocks add: block.
		block := block next ].

	^ blocks
]

{ #category : 'abstract interpretation' }
ASCBasicBlock >> branch: anASCInlineBlock [

	self put: (ASCBranch with: anASCInlineBlock).
	anASCInlineBlock setStack: stack.
	^ anASCInlineBlock
]

{ #category : 'accessing' }
ASCBasicBlock >> children [
	^ operations
]

{ #category : 'abstract interpretation' }
ASCBasicBlock >> classCase: aCollection [

	| case |
	case := ASCCase new.
	aCollection do: [ :class |
		| next |
		next := ASCInlineBlock for: self.
		case at: class put: next.
		next addSource: self.
		next setStack: stack ].
	self put: case.
	^ case
]

{ #category : 'data flow instructions' }
ASCBasicBlock >> classForLocals [

	^ self subclassResponsibility
]

{ #category : 'data flow instructions' }
ASCBasicBlock >> classForTemporaries [

	^ self subclassResponsibility
]

{ #category : 'stackAccessHelpers' }
ASCBasicBlock >> doCall: aSymbol [

	^ self doDrop: aSymbol numArgs
]

{ #category : 'stackAccessHelpers' }
ASCBasicBlock >> doDrop [

	^ self doDrop: 1
]

{ #category : 'stackAccessHelpers' }
ASCBasicBlock >> doDrop: n [

	stack := stack drop: n.
	stackDepth := stackDepth - n
]

{ #category : 'stackAccessHelpers' }
ASCBasicBlock >> doDup [

	self needs: stack top source.
	stack := stack from: stack top.
	self error: 'incorrect implementation'
]

{ #category : 'stackAccessHelpers' }
ASCBasicBlock >> doPop [

	| result |
	result := stack value.
	stack := stack drop.
	^ result
]

{ #category : 'stackAccessHelpers' }
ASCBasicBlock >> doPush: anObject [

	stack := stack push: anObject.
	maxDepth := maxDepth max: (stackDepth := stackDepth + 1)
]

{ #category : 'abstract interpretation' }
ASCBasicBlock >> embed: aSymbol return: aClass [

	| sendEmbed |
	sendEmbed := ASCEmbed with: aSymbol.
	self put: sendEmbed.
	"we should know the type! can push something more than unknown here"
	self
		stack: ((stack drop: aSymbol numArgs + 1)
		push: (ASCClassValue new init: aClass)).
	^ nil
]

{ #category : 'initialization' }
ASCBasicBlock >> endWithReturnTop [

	^ self put: ASCReturnTop new
]

{ #category : 'accessing' }
ASCBasicBlock >> find: aSymbol [

	^ stack find: aSymbol
]

{ #category : 'accessing' }
ASCBasicBlock >> flow [

	^ false
]

{ #category : 'comparing' }
ASCBasicBlock >> hash [

	^ hash
]

{ #category : 'comparing' }
ASCBasicBlock >> hash: value [

	hash := value
]

{ #category : 'stackAccessHelpers' }
ASCBasicBlock >> indexOf: anObject [
"delegate to stack?"
	^ stack offset: 0 to: anObject 

]

{ #category : 'initialization' }
ASCBasicBlock >> initialize [

	super initialize.
	operations := OrderedCollection new.
	stackDepth := maxDepth := 0.
	stack := ASStack new.
	hash := SequenceNumber := SequenceNumber + 1.
	sources := OrderedCollection new
]

{ #category : 'printing' }
ASCBasicBlock >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	aStream
		<< $(;
		<< self label;
		<< $)
]

{ #category : 'testing' }
ASCBasicBlock >> isRecursive: aCollection [ 
	self shouldBeImplemented.
]

{ #category : 'initialization' }
ASCBasicBlock >> label [

	^ self subclassResponsibility
]

{ #category : 'output' }
ASCBasicBlock >> llvmLabel [
	^ self subclassResponsibility
]

{ #category : 'API' }
ASCBasicBlock >> makeContextRequired [

	context ifNil: [
		stack := stack makeContextRequired: [ :c | context := c ] ]
]

{ #category : 'inlining' }
ASCBasicBlock >> markBlockClosuresToManifest [

	| last |
	operations ifNotEmpty: [ 
	last := operations last.
	last requiresContext ifTrue: [ self makeContextRequired.
 ].
	last manifestParameters: stack
	]
]

{ #category : 'accessing' }
ASCBasicBlock >> maxDepth [
	^ maxDepth
]

{ #category : 'accessing' }
ASCBasicBlock >> method: ignored [

	^ self subclassResponsibility
]

{ #category : 'data flow instructions' }
ASCBasicBlock >> namedMethod [
	^ self subclassResponsibility
]

{ #category : 'accessing' }
ASCBasicBlock >> next [
	^ operations ifNotEmpty: [ operations last next ] ifEmpty: [ nil ]
]

{ #category : 'as yet unclassified' }
ASCBasicBlock >> nextInline [

	^ nil
]

{ #category : 'successor sequences' }
ASCBasicBlock >> nextSequence [
	^ self subclassResponsibility
]

{ #category : 'instance initialization' }
ASCBasicBlock >> nonlocalNameLookupObject: aGenerator [

	stack nonlocalNameLookupObject: aGenerator
]

{ #category : 'API' }
ASCBasicBlock >> offsetOf: aSymbol context: blockLocal stack: blockStack other: blockOther [

	^ stack
		  offset: 0
		  to: aSymbol
		  context: blockLocal
		  stack: blockStack
		  other: blockOther
]

{ #category : 'initialization' }
ASCBasicBlock >> offsetTo: anObject [

	^ stack offset: 0 to: anObject
]

{ #category : 'initialization' }
ASCBasicBlock >> offsetTo: anObject methodBlock: aBlock [
	aBlock value: label.
	 ^ stack offset: 0 to: anObject
]

{ #category : 'writing' }
ASCBasicBlock >> outputRestTo: output [
	operations do: [ :operation | operation outputTo: output ].
	^ output outputBasicBlockEnd
]

{ #category : 'writing' }
ASCBasicBlock >> outputTo: output [

	output outputMethodStart: self label.
	output outputMethodLabel: label.
	^ self outputRestTo: output
]

{ #category : 'API' }
ASCBasicBlock >> popAndCopyTopToSelf [
	" This is tricky code because we need to save all the properties of the top of stack and over-write the `self` that was part of the inlining. So we throw away everything on the stack up to and including that `self` value, and then return a stack with the original top element linking to whatever was below. "

	| count top |
	count := stack offsetToSelf: 0.
	self put: (ASCPopAndCopyTop with: count).
	top := stack.
	stack := top fromStack: (stack drop: count + 1)
]

{ #category : 'API' }
ASCBasicBlock >> popVariable: aSymbol [

	| target |
	target := stack find: aSymbol.
	stack top makeManifest.
	self put: (ASCPopVariable with: target top).
	self doDrop
]

{ #category : 'API' }
ASCBasicBlock >> pushBlockNamed: aBlockClosure [

	stack := (ASStackNamed fromStack: stack)
		         name: aBlockClosure name.
	^ stack top: aBlockClosure
]

{ #category : 'API' }
ASCBasicBlock >> pushLocalNamed: aSymbol basicBlock: basicBlock [

	stack := stack pushLocalNamed: aSymbol basicBlock: self.
	^ stack
		  value: (ASCLocal new
				   init: Object;
				   name: aSymbol);
		  source: basicBlock classForLocals new
]

{ #category : 'API' }
ASCBasicBlock >> pushParameterNamed: aSymbol [

	| source |
	stack := stack pushParameterNamed: aSymbol.
	source := ASFlowDefine new id: aSymbol.
	sources add: source.
	^ stack value: (ASCParameter new
			   init: Object;
			   name: aSymbol);
			source: source
]

{ #category : 'API' }
ASCBasicBlock >> pushVariable: aSymbol [

	| target |
	target := stack at: aSymbol .
	self put: (ASCPushVariable with: target value).
	self doPush: target value
]

{ #category : 'initialization' }
ASCBasicBlock >> put: anObject [

	^ operations add: anObject
]

{ #category : 'API' }
ASCBasicBlock >> putSimple: aSymbol [ 
	^ self put: (ASCSimple with: aSymbol)
]

{ #category : 'accessing' }
ASCBasicBlock >> reference: anIntegerOrSymbol [

	(stack at: anIntegerOrSymbol) source markReferenced
]

{ #category : 'enumerating' }
ASCBasicBlock >> requiredContextDo: aBlock [

	context ifNotNil: [ : c | aBlock value: c ]
]

{ #category : 'initialization' }
ASCBasicBlock >> returnNonLocal [

	^ self put: ASCReturnNonLocal new
]

{ #category : 'initialization' }
ASCBasicBlock >> returnSelf [

	^ self put: ASCReturnSelf new
]

{ #category : 'initialization' }
ASCBasicBlock >> returnTop [

	^ self put: ASCReturnTop new
]

{ #category : 'initialization' }
ASCBasicBlock >> roots [

	| roots |
	roots := OrderedCollection new.
	roots add: self.
	roots addAll: self blocks.

	^ roots
]

{ #category : 'abstract interpretation' }
ASCBasicBlock >> send: aSymbol targetClass: target tail: tailCall [

	| next send |
	next := tailCall ifFalse: [ ASCReturnBlock for: self ].
	send := ASCSend new
		        selector: aSymbol;
		        return: next;
		        target: target.
	self put: send.
	next ifNotNil: [
		next setStack: (stack drop: aSymbol numArgs + 1) pushUnknown ].
	^ next
]

{ #category : 'accessing' }
ASCBasicBlock >> setStack: anASStack [

	stack := anASStack.
	^ stack source: 0
]

{ #category : 'accessing' }
ASCBasicBlock >> stack [
	^ stack
]

{ #category : 'accessing' }
ASCBasicBlock >> stack: anASStack [

	stack := anASStack.
	maxDepth := stack depth
]

{ #category : 'stackAccessHelpers' }
ASCBasicBlock >> stackTop [
	^ stack top
]

{ #category : 'data flow instructions' }
ASCBasicBlock >> storeInto: offsetOrName [

	| target source |
	target := stack at: offsetOrName.
	source := ASFlowNew new basicBlock: self.
	target source: source.
	^ source
]

{ #category : 'API' }
ASCBasicBlock >> storeVariable: aSymbol [

	stack top makeManifest.
	self put: (ASCStoreVariable with: (stack find: aSymbol) top)
]

{ #category : 'accessing' }
ASCBasicBlock >> top [
	^ stack top
]

{ #category : 'data flow instructions' }
ASCBasicBlock >> updateDependencies [

	^ #()
	
]
