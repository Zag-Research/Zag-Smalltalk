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
		'index'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

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

{ #category : 'inlining' }
ASCBasicBlock >> canBeInlinedBy: anASCodeGenerator [
| aSend case |
	aSend := operations last.
	aSend isSend ifFalse: [ ^ false ].
	
	case := ASCCase new.
	"case needs to know the relationship between classes and block name!"
	operations add: case before: operations last.
	"check if the send can be inlined"
	^ anASCodeGenerator inline: aSend
]

{ #category : 'accessing' }
ASCBasicBlock >> children [
	^ operations
]

{ #category : 'abstract interpretation' }
ASCBasicBlock >> classCase: aCollection complete: aBoolean [

	| case |
	case := ASCCase new complete: aBoolean.
	aCollection do: [ :class |
		| next |
		next := ASCInlineBlock new.
		case at: class put: next.
		next stack: stack ].
	self put: case.
	^ case
]

{ #category : 'stackAccessHelpers' }
ASCBasicBlock >> doCall: aSymbol [

	stack := stack drop: aSymbol numArgs
]

{ #category : 'stackAccessHelpers' }
ASCBasicBlock >> doDrop [
	stack := stack drop
]

{ #category : 'stackAccessHelpers' }
ASCBasicBlock >> doDrop: n [

	stack := stack drop: n.

]

{ #category : 'stackAccessHelpers' }
ASCBasicBlock >> doDup [
	stack := stack from: stack top 
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
	maxDepth := maxDepth max: stack depth
]

{ #category : 'abstract interpretation' }
ASCBasicBlock >> embed: aSymbol [

	| next send |
	next := ASCReturnBlock new.
	send := ASCEmbed new
		        selector: aSymbol;
		        return: next;
		        target: nil.
	self put: send.
	next ifNotNil: [
		next stack: (stack drop: aSymbol numArgs + 1) pushUnknown ].
	^ next
]

{ #category : 'initialization' }
ASCBasicBlock >> endWithReturnTop [

	^ self put: ASCReturnTop new
]

{ #category : 'accessing' }
ASCBasicBlock >> find: aSymbol [

	^ (stack find: aSymbol) push: aSymbol on: stack
]

{ #category : 'accessing' }
ASCBasicBlock >> index [
	^ index
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
	maxDepth := 0.
	stack := ASEmptyStack new.
]

{ #category : 'printing' }
ASCBasicBlock >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	aStream
		<< $(;
		<< label;
		<< $)
]

{ #category : 'initialization' }
ASCBasicBlock >> label [
	^ label
]

{ #category : 'initialization' }
ASCBasicBlock >> label: aString index: anInteger [

	label := aString .
	index := anInteger 
]

{ #category : 'API' }
ASCBasicBlock >> makeContextRequired [
	stack := stack makeContextRequired 
]

{ #category : 'inlining' }
ASCBasicBlock >> markBlockClosuresToManifest [

	| last |
	operations ifNotEmpty: [ 
	last := operations last.
	last requiresContext ifTrue: [ stack := stack makeContextRequired.
 ].
	last manifestParameters: stack
	]
]

{ #category : 'accessing' }
ASCBasicBlock >> maxDepth [
	^ maxDepth
]

{ #category : 'accessing' }
ASCBasicBlock >> next [
	^ operations ifNotEmpty: [ operations last next ] ifEmpty: [ nil ]
]

{ #category : 'instance initialization' }
ASCBasicBlock >> nonlocalNameLookupObject: aGenerator [

	stack nonlocalNameLookupObject: aGenerator
]

{ #category : 'API' }
ASCBasicBlock >> offsetOf: anObject local: blockLocal stack: blockStack other: blockOther [
	^ stack
		  offset: 0
		  to: anObject
		  local: blockLocal
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

	output outputMethodStart: label.
	output outputMethodLabel: label.
	^ self outputRestTo: output
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

	stack := ASStackNamed new
		         from: stack;
		         name: aBlockClosure name.
	^ stack top: aBlockClosure
]

{ #category : 'API' }
ASCBasicBlock >> pushLocalNamed: aSymbol [

	stack := ASStackNamed new
		         from: stack;
		         name: aSymbol.
	^ stack top: (ASCLocal new init: Object;
			   name: aSymbol)
]

{ #category : 'API' }
ASCBasicBlock >> pushParameterNamed: aSymbol [

	stack := ASStackNamed new
		         from: stack;
		         name: aSymbol.
	^ stack top: (ASCParameter new init: Object; name: aSymbol)
]

{ #category : 'API' }
ASCBasicBlock >> pushVariable: aSymbol [

	| target |
	target := stack find: aSymbol .
	self put: (ASCPushVariable with: target top).
	self doPush: target top
]

{ #category : 'initialization' }
ASCBasicBlock >> put: anObject [

	^ operations add: anObject
]

{ #category : 'API' }
ASCBasicBlock >> putSimple: aSymbol [ 
	^ self put: (ASCSimple with: aSymbol)
]

{ #category : 'enumerating' }
ASCBasicBlock >> requiredContextDo: aBlock [
stack requiredContextDo: aBlock 
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
	next := tailCall ifFalse: [ ASCReturnBlock new ].
	send := ASCSend new
		        selector: aSymbol;
		        return: next;
		        target: target.
	self put: send.
	next ifNotNil: [
		next stack: (stack drop: aSymbol numArgs + 1) pushUnknown ].
	^ next
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

{ #category : 'API' }
ASCBasicBlock >> storeVariable: aSymbol [

	stack top makeManifest.
	self put: (ASCStoreVariable with: (stack find: aSymbol) top)
]

{ #category : 'accessing' }
ASCBasicBlock >> top [
	^ stack top
]
