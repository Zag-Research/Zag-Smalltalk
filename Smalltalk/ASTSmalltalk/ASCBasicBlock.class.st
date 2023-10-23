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
		'index',
		'end',
		'next'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #API }
ASCBasicBlock >> assignLocal: anASLocal with: aGenerator [
stack top manifest: aGenerator .
	self put: (ASCStoreVariable with: anASLocal)
]

{ #category : #initialization }
ASCBasicBlock >> childeren [
	| childeren |
	childeren := OrderedCollection new addAll: operations.
 	next ifNotNil: [ childeren add: next].
	
	^childeren
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

{ #category : #accessing }
ASCBasicBlock >> end: anASCOperation [ 
	end := anASCOperation
]

{ #category : #initialization }
ASCBasicBlock >> endWithReturnTop [

		end := ASCReturnTop new
]

{ #category : #accessing }
ASCBasicBlock >> index [
	^ index
]

{ #category : #initialization }
ASCBasicBlock >> initialize [

	super initialize.
	operations := OrderedCollection new.
	end := ASCReturnSelf new 
]

{ #category : #accessing }
ASCBasicBlock >> inspectBlock [

	<inspectorPresentationOrder: 0 title: 'BLOCKS'>
	^ SpTreePresenter new
		  roots: { self };
		  children: [ :aNode | aNode childeren ];
		  display: [ :each |
			  String streamContents: [ :stream |
					  each printOn: stream ] ];
		  yourself
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

{ #category : #accessing }
ASCBasicBlock >> maxDepth [
	^ maxDepth
]

{ #category : #accessing }
ASCBasicBlock >> next [
	^ next
]

{ #category : #API }
ASCBasicBlock >> offsetOf: anObject local: localBlock stack: stackBlock [

	^ stack offset: 0
		  to: anObject
		  local: localBlock
		  stack: localBlock
]

{ #category : #wriiting }
ASCBasicBlock >> outputRestTo: output [

	operations do: [ :operation | operation outputTo: output ].
	end outputTo: output.
	output outputMethodEnd
]

{ #category : #wriiting }
ASCBasicBlock >> outputTo: output [

	self subclassResponsibility 
]

{ #category : #API }
ASCBasicBlock >> popLocal: anASLocal [

	self put: (ASCPopVariable with: anASLocal).
	self doDrop
]

{ #category : #accessing }
ASCBasicBlock >> printOn: aStream [
	aStream
		nextPutAll: self class name;
		nextPut: $(.
		self class slots do: [ :aSlot | 
			aStream nextPutAll: aSlot name;
			nextPutAll: ' => '.
			(self instVarNamed: aSlot name) printOn: aStream
		] separatedBy: [ aStream space ].
		aStream nextPut: $)
]

{ #category : #API }
ASCBasicBlock >> pushLocal: anASCLocal [

	| target |
	target := stack find: anASCLocal name.
	self put: (ASCPushVariable with: anASCLocal ).
	self doPush: target top
]

{ #category : #API }
ASCBasicBlock >> pushUnknownNamed: aSymbol [

	stack := ASStackNamed new
		         from: stack;
		         name: aSymbol.
	stack top: (ASCClassValue new init: Object)
]

{ #category : #initialization }
ASCBasicBlock >> put: anObject [
	operations add: anObject 
]

{ #category : #enumerating }
ASCBasicBlock >> requiredContextDo: aBlock [
stack requiredContextDo: aBlock 
]

{ #category : #initialization }
ASCBasicBlock >> returnTop [
		self doDrop.
		end := ASCReturnTop new

]

{ #category : #'abstract interpretation' }
ASCBasicBlock >> send: aSymbol targetClass: target tail: tailCall [

	next := ASCReturnBlock new
		        end: end.
	end := ASCSend new
		       selector: aSymbol;
		       return: next;
		       target: target;
		       tail: tailCall.
	stack := stack makeContextRequired.
	next 
		        stack: stack.
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

{ #category : #accessing }
ASCBasicBlock >> top [
	^ stack top
]
