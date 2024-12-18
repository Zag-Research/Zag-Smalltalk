"
I represent an object on the stack that we know nothing about.

Because we can make no assumptions, I can represent anything.
"
Class {
	#name : 'AStSubClass',
	#superclass : 'AStExactClass',
	#category : 'Zag-Core-Stack',
	#package : 'Zag-Core',
	#tag : 'Stack'
}

{ #category : 'as yet unclassified' }
AStSubClass >> couldMergeIn: other [

	^ other isKindOf: self class
]

{ #category : 'as yet unclassified' }
AStSubClass >> dataClass [

	^ Object
]
