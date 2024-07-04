"
I represent an expression that is a message send.
"
Class {
	#name : 'ASSend',
	#superclass : 'ASExpression',
	#instVars : [
		'target',
		'selector',
		'args'
	],
	#category : 'ASTSmalltalk-AST',
	#package : 'ASTSmalltalk',
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

{ #category : 'parsing' }
ASSend >> args [
	^ args
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

{ #category : 'printing' }
ASSend >> printOn: s [
	s << self className.
	target ifNotNil: [ 
		s << ' target: ('.
		target printOn: s.
		s << ')' ].
	s << ' selector: '.
	selector printOn: s.
	s << ' args: {'.
	args do: [ :arg | arg printOn: s ] separatedBy: [ s << ' . ' ].
	s << '}'
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
ASSend >> zigWalk: aGenerator [

	^ self zigWalk: aGenerator tail: false
]

{ #category : 'compiling' }
ASSend >> zigWalk: aGenerator tail: tail [

	| myTarget |
	myTarget := target zigWalkTarget: aGenerator.
	" aGenerator topOfStackType." "is this needed or just walk target?"
	args do: [ :arg | arg zigWalk: aGenerator ].
	aGenerator send: selector targetClass: myTarget tail: tail.
	^ tail
]

{ #category : 'compiling' }
ASSend >> zigWalkForReturn: aGenerator [

	^ self zigWalk: aGenerator  tail: true
]

{ #category : 'compiling' }
ASSend >> zigWalkInlineMethodReturn: aGenerator [

	^ self zigWalk: aGenerator tail: true
]

{ #category : 'compiling' }
ASSend >> zigWalkTarget: aGenerator [

	self zigWalk: aGenerator.
	^ nil
]
