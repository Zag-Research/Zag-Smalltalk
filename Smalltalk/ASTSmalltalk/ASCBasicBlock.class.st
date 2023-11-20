"
I represent a Basic Block for ASCodeGenerator.

Every Method or block is made up of a graph of these.

I have a `then` field which is the block I will go to if the named test is true.

If there is no test, or if the test is false, then I will continue with the `else` block.
"
Class {
	#name : #ASCBasicBlock,
	#superclass : #Object,
	#instVars : [
		'stack',
		'maxDepth',
		'operations',
		'label',
		'index'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #accessing }
ASCBasicBlock >> blocks [

	| blocks block |
	blocks := OrderedCollection new.
	block := self next.
	[ block notNil ] whileTrue: [
		blocks add: block.
		block := block next ].

	^ blocks
]

{ #category : #inlining }
ASCBasicBlock >> canBeInlinedBy: anASCodeGenerator [ 
	^ false
]

{ #category : #accessing }
ASCBasicBlock >> children [
	^ operations
]

{ #category : #stackAccessHelpers }
ASCBasicBlock >> doCall: aSymbol [

	stack := stack drop: aSymbol numArgs
]

{ #category : #stackAccessHelpers }
ASCBasicBlock >> doDrop [
	stack := stack drop
]

{ #category : #stackAccessHelpers }
ASCBasicBlock >> doDrop: n [

	stack := stack drop: n.

]

{ #category : #stackAccessHelpers }
ASCBasicBlock >> doPop [

	| result |
	result := stack value.
	stack := stack drop.
	^ result
]

{ #category : #stackAccessHelpers }
ASCBasicBlock >> doPush: anObject [

	stack := stack push: anObject.
	maxDepth := maxDepth max: stack depth
]

{ #category : #initialization }
ASCBasicBlock >> endWithReturnTop [

	^ self put: ASCReturnTop new
]

{ #category : #accessing }
ASCBasicBlock >> index [
	^ index
]

{ #category : #initialization }
ASCBasicBlock >> initialize [

	super initialize.
	operations := OrderedCollection new.
	maxDepth := 0
]

{ #category : #initialization }
ASCBasicBlock >> label [
	^ label
]

{ #category : #initialization }
ASCBasicBlock >> label: aString index: anInteger [

	label := aString .
	index := anInteger 
]

{ #category : #API }
ASCBasicBlock >> makeContextRequired [
	stack := stack makeContextRequired 
]

{ #category : #inlining }
ASCBasicBlock >> markBlockClosuresToManifest [

	| last |
	last := operations last.
	last requiresContext ifTrue: [ stack := stack makeContextRequired.
 ].
	last manifestParameters: stack
]

{ #category : #accessing }
ASCBasicBlock >> maxDepth [
	^ maxDepth
]

{ #category : #accessing }
ASCBasicBlock >> next [
	^ operations last next
]

{ #category : #API }
ASCBasicBlock >> offsetOf: anObject local: localBlock stack: stackBlock other: blockOther [

	^ stack
		  offset: 0
		  to: anObject
		  local: localBlock
		  stack: localBlock
		  other: nil
]

{ #category : #writing }
ASCBasicBlock >> outputRestTo: output [

	operations do: [ :operation | operation outputTo: output ].
	^ output outputBasicBlockEnd
]

{ #category : #writing }
ASCBasicBlock >> outputTo: output [

	self subclassResponsibility 
]

{ #category : #API }
ASCBasicBlock >> popVariable: anASVariable [

	stack top makeManifest.
	self put: (ASCPopVariable with: anASVariable).
	self doDrop
]

{ #category : #API }
ASCBasicBlock >> pushLocalNamed: aSymbol [

	stack := ASStackNamed new
		         from: stack;
		         name: aSymbol.
	^ stack top: (ASCLocal new init: Object)
]

{ #category : #API }
ASCBasicBlock >> pushParameterNamed: aSymbol [

	stack := ASStackNamed new
		         from: stack;
		         name: aSymbol.
	^ stack top: (ASCParameter new init: Object)
]

{ #category : #API }
ASCBasicBlock >> pushVariable: aSymbol [

	| target |
	target := stack find: aSymbol .
	self put: (ASCPushVariable with: aSymbol).
	self doPush: target top
]

{ #category : #initialization }
ASCBasicBlock >> put: anObject [

	^ operations add: anObject
]

{ #category : #API }
ASCBasicBlock >> putSimple: aSymbol [ 
	^ self put: (ASCSimple with: aSymbol)
]

{ #category : #enumerating }
ASCBasicBlock >> requiredContextDo: aBlock [
stack requiredContextDo: aBlock 
]

{ #category : #initialization }
ASCBasicBlock >> returnSelf [

	^ self put: ASCReturnSelf new
]

{ #category : #initialization }
ASCBasicBlock >> returnTop [

	^ self put: ASCReturnTop new
]

{ #category : #initialization }
ASCBasicBlock >> roots [

	| roots |
	roots := OrderedCollection new.
	roots add: self.
	roots addAll: self blocks.

	^ roots
]

{ #category : #'abstract interpretation' }
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

{ #category : #accessing }
ASCBasicBlock >> stack [
	^ stack
]

{ #category : #accessing }
ASCBasicBlock >> stack: anASStack [

	stack := anASStack.
	maxDepth := stack depth
]

{ #category : #stackAccessHelpers }
ASCBasicBlock >> stackTop [
	^ stack top
]

{ #category : #API }
ASCBasicBlock >> storeVariable: aSymbol [

	stack top makeManifest.
	self put: (ASCStoreVariable with: (stack lookup: aSymbol))
]

{ #category : #accessing }
ASCBasicBlock >> top [
	^ stack top
]
