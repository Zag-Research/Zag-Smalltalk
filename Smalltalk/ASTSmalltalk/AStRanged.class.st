"
I represent a `SmallInteger` that we know something about.

Ideally we have a tight bound on the value, but either field can be `nil` to say we know nothing about that side.
"
Class {
	#name : 'AStRanged',
	#superclass : 'AStSmallInteger',
	#instVars : [
		'geThan',
		'leThan'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'as yet unclassified' }
AStRanged >> couldMergeIn: other [

	self class ~~ other class ifTrue: [ ^ false ].
	self geThan >= other geThan ifFalse: [ ^ false ].
	^ self leThan <= other leThan
]

{ #category : 'accessing' }
AStRanged >> geThan [

	^ geThan
]

{ #category : 'accessing' }
AStRanged >> geThan: anObject [

	geThan := anObject
]

{ #category : 'accessing' }
AStRanged >> leThan [

	^ leThan
]

{ #category : 'accessing' }
AStRanged >> leThan: anObject [

	^ leThan := anObject
]

{ #category : 'initialization' }
AStRanged >> set: anInteger [ 

	geThan := leThan := anInteger
]

{ #category : 'initialization' }
AStRanged >> setValue: aSmallInteger [

	geThan := leThan := aSmallInteger
]
