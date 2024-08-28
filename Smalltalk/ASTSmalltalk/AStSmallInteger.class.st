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

{ #category : 'initialization' }
AStSmallInteger >> setValue: aSmallInteger [

	geThan := leThan := aSmallInteger
]
