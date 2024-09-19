"
I represent a `SmallInteger` value, but nothing else is known.

My subclasses represent `SmallInteger`s that we know something about.
"
Class {
	#name : 'AStSmallInteger',
	#superclass : 'AStExactClass',
	#instVars : [
		'geThan',
		'leThan'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'accessing' }
AStSmallInteger class >> geThan: anInteger [

	^ self new geThan: anInteger
]

{ #category : 'initialization' }
AStSmallInteger class >> setValue: anInteger [

	^ self new setValue: anInteger
]

{ #category : 'arithmetic' }
AStSmallInteger >> + anInteger [

	anInteger = 0 ifTrue: [ ^ self ].
	anInteger > 0 ifTrue: [	^ self class new geThan: geThan + anInteger ].
	anInteger < 0 ifTrue: [	^ self class new leThan: leThan + anInteger ]
]

{ #category : 'comparing' }
AStSmallInteger >> = other [

	^ leThan = other leThan and: [ geThan = other geThan ]
]

{ #category : 'arithmetic' }
AStSmallInteger >> adaptToNumber: anInteger andSend: aSymbol [

	aSymbol = #+ ifTrue: [ ^ self + anInteger ].
	^ super adaptToNumber: anInteger andSend: aSymbol
]

{ #category : 'as yet unclassified' }
AStSmallInteger >> couldMergeIn: other [

	| sc oc |
	sc := self class.
	oc := other class.
	sc ~~ oc ifTrue: [
		sc ~~ oc superclass  ifTrue: [ ^ false ].
		self halt ].
	geThan >= other geThan ifFalse: [ ^ false ].
	^ leThan <= other leThan
]

{ #category : 'accessing' }
AStSmallInteger >> geThan [

	^ geThan
]

{ #category : 'accessing' }
AStSmallInteger >> geThan: anObject [

	geThan := anObject
]

{ #category : 'initialization' }
AStSmallInteger >> initialize [

	super initialize.
	dataClass := SmallInteger
]

{ #category : 'accessing' }
AStSmallInteger >> leThan [

	^ leThan
]

{ #category : 'accessing' }
AStSmallInteger >> leThan: anObject [

	^ leThan := anObject
]

{ #category : 'accessing' }
AStSmallInteger >> maxWith: anInteger [

	^ leThan ifNotNil: [ anInteger ifNotNil: [leThan max: anInteger]]
]

{ #category : 'API' }
AStSmallInteger >> mergeIn: other [

	^ self class new geThan: (self minWith: other geThan);
	leThan: (self maxWith: other leThan);yourself 
]

{ #category : 'accessing' }
AStSmallInteger >> minWith: anInteger [

	^ geThan min: anInteger
]

{ #category : 'initialization' }
AStSmallInteger >> setValue: aSmallInteger [

	geThan := leThan := aSmallInteger
]
