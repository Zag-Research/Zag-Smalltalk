"
I represent an expression that is a message send.

My arguments are:
- the target object that the message is being sent to (subtree)
- the selector of the message being sent, as a symbol
- the arguments passed to this message, besides the target.
"
Class {
	#name : 'ASSend',
	#superclass : 'ASExpression',
	#instVars : [
		'target',
		'selector',
		'args'
	],
	#category : 'Zag-Core-AST',
	#package : 'Zag-Core',
	#tag : 'AST'
}

{ #category : 'instance creation' }
ASSend class >> selector: aSymbol args: aCollection [

	^ self new selector: aSymbol args: aCollection
]

{ #category : 'instance creation' }
ASSend class >> target: aTarget selector: aSymbol [

	^ self new target: aTarget; selector: aSymbol 
]

{ #category : 'instance creation' }
ASSend class >> target: expression selector: aSymbol args: aCollection [

	^ self new target: expression; selector: aSymbol args: aCollection
]

{ #category : 'comparing' }
ASSend >> = other [

	^ self class = other class and: [ 
		  self selector = other selector and: [ 
			  self target = other target and: [ self args = other args ] ] ]
]

{ #category : 'visiting' }
ASSend >> accept: aVisitor [
	^aVisitor visitSend: self
]

{ #category : 'compiling' }
ASSend >> argNeedsBrackets: anArg [
	"Accepts one of my (non-target) arguments, and returns whether or not it needs brackets."
	(anArg isKindOf: ASStatement) ifFalse: [ ^false ].
	anArg isASAssign ifTrue: [ ^true ].
	anArg isASSend ifFalse: [ ^false ].
	^self
		ifUnary: [ self error: 'Unary messages have no arguments!' ]
		ifBinary: [ anArg messageType ~= #unary ]
		ifKeyword: [ anArg messageType = #keyword ].
]

{ #category : 'parsing' }
ASSend >> args [
	^ args
]

{ #category : 'converting' }
ASSend >> asPharoAST [
	| pharoArgs |
	pharoArgs := args ifNil: [ #() ] ifNotNil: [ args collect: #asPharoAST ].
	^OCMessageNode receiver: target asPharoAST selector: selector arguments: pharoArgs.
]

{ #category : 'accessing' }
ASSend >> blocks [

	| blocks |
	blocks := OrderedCollection new.
	args ifNotNil: [ args do: [ :arg | blocks addAll: arg blocks ] ].
	target isASBlock ifTrue: [ blocks addAll: target blocks ].
	^ blocks
]

{ #category : 'accessing' }
ASSend >> children [

	| childeren |
	childeren := OrderedCollection new.
	target ifNotNil: [ :t | childeren add: t ].
	childeren addAll: args.
	^ childeren
]

{ #category : 'parsing' }
ASSend >> ifUnary: unaryBlock ifBinary: binaryBlock ifKeyword: keywordBlock [
	"Runs one of the provided blocks based on my type."
	| myType |
	myType := self messageType.
	myType = #unary ifTrue: [ ^unaryBlock value ].
	myType = #binary ifTrue: [ ^binaryBlock value ].
	myType = #keyword ifTrue: [ ^keywordBlock value ].
	self error: 'Invalid message type'
]

{ #category : 'printing' }
ASSend >> inspectZagASTOn: s [

	super inspectZagASTOn: s.
	s << $(.
	selector printOn: s.
	s << $)
]

{ #category : 'parsing' }
ASSend >> isASSend [
	^true
]

{ #category : 'parsing' }
ASSend >> maxDepth [
	^selector numArgs
]

{ #category : 'parsing' }
ASSend >> messageType [
	"Returns whether or not I am unary, binary or keyword."
	| selectorString |
	selectorString := self selector asString.
	(selectorString includes: $:) ifTrue: [ ^#keyword ].
	^(selectorString allSatisfy: #isSpecial)
		ifTrue: [ #binary ] ifFalse: [ #unary ].
]

{ #category : 'parsing' }
ASSend >> selector [
	^ selector
]

{ #category : 'accessing' }
ASSend >> selector: aSelector [
	selector := aSelector
]

{ #category : 'parsing' }
ASSend >> selector: aSymbol args: aCollection [

	selector := aSymbol.
	args := aCollection
]

{ #category : 'parsing' }
ASSend >> target [

	^ target
]

{ #category : 'parsing' }
ASSend >> target: expression [
	target := expression
]

{ #category : 'compiling' }
ASSend >> targetNeedsBrackets [
	"Returns whether or not my target needs to be surrounded by brackets in source code."
	(self target isKindOf: ASStatement) ifFalse: [ ^false ].
	self target isASAssign ifTrue: [ ^true ].
	self target isASSend ifFalse: [ ^false ].
	^self
		ifUnary: [ self target messageType ~= #unary ]
		ifBinary: [ self target messageType = #keyword ]
		ifKeyword: [ self target messageType = #keyword ].
]

{ #category : 'compiling' }
ASSend >> zigWalk: aGenerator [

	^ self zigWalk: aGenerator tail: false
]

{ #category : 'compiling' }
ASSend >> zigWalk: aGenerator tail: tail [

	| myTarget |
	myTarget := target zigWalkTarget: aGenerator.
	args do: [ :arg | arg zigWalk: aGenerator ].
	aGenerator send: selector targetClass: myTarget tail: tail.
	^ tail
]

{ #category : 'compiling' }
ASSend >> zigWalkForReturn: aGenerator [

	^ self zigWalk: aGenerator  tail: true
]
