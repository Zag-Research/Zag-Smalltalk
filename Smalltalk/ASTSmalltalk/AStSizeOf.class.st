"
I rempresent an integer that is know to be the size of some other object +/- some offset.

This can be used by `at:` or `at:put:` to be know to be safe.
"
Class {
	#name : 'AStSizeOf',
	#superclass : 'AStSmallInteger',
	#instVars : [
		'object',
		'delta'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'accessing' }
AStSizeOf class >> object: anObject [

	^ self new object: anObject 
]

{ #category : 'arithmetic' }
AStSizeOf >> delta: anInteger [

	delta := anInteger
]

{ #category : 'accessing' }
AStSizeOf >> geThan [

	^ self
]

{ #category : 'initialization' }
AStSizeOf >> initialize [

	super initialize.
	delta := 0
]

{ #category : 'accessing' }
AStSizeOf >> leThan [

	^ self
]

{ #category : 'accessing' }
AStSizeOf >> object: anObject [

	object := anObject
]
