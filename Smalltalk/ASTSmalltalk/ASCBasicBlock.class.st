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
		'test',
		'else',
		'then'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #adding }
ASCBasicBlock >> addTo: aCollection [ 
	self shouldBeImplemented.
]

{ #category : #stackAccessHelpers }
ASCBasicBlock >> doCall: aSymbol [

	stack := stack drop: aSymbol numArgs
]

{ #category : #stackAccessHelpers }
ASCBasicBlock >> doDrop [

	stack := stack drop.

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
ASCBasicBlock >> else: anInteger [

	else := anInteger
]

{ #category : #initialization }
ASCBasicBlock >> ifFalseGoTo: aBasicBlock [

	then := aBasicBlock.
	test := #ifFalse:
]

{ #category : #initialization }
ASCBasicBlock >> ifNilGoTo: aBasicBlock [

	then := aBasicBlock.
	test := #ifNil:
]

{ #category : #initialization }
ASCBasicBlock >> ifNotNilGoTo: aBasicBlock [

	then := aBasicBlock.
	test := #ifNotNil:
]

{ #category : #initialization }
ASCBasicBlock >> ifTrueGoTo: aBasicBlock [

	then := aBasicBlock.
	test := #ifTrue:
]

{ #category : #initialization }
ASCBasicBlock >> initialize [

	super initialize.
	operations := OrderedCollection new
]

{ #category : #initialization }
ASCBasicBlock >> label: anObject [

	label := anObject
]

{ #category : #wriiting }
ASCBasicBlock >> outputTo: output [ 
	operations do: [ : operation | operation outputTo: output ]
]

{ #category : #initialization }
ASCBasicBlock >> put: anObject [
	operations add: anObject 
]

{ #category : #initialization }
ASCBasicBlock >> returnTop [
test := #return.
	self put: (#returnTop)
]

{ #category : #accessing }
ASCBasicBlock >> stack: anASStack [

	stack := anASStack.
	maxDepth := stack depth
]

{ #category : #initialization }
ASCBasicBlock >> then: anInteger [

	else := anInteger.

]
