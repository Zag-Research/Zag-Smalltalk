"
I represent a `SmallInteger` value, but nothing else is known.

My subclasses represent `SmallInteger`s that we know something about.
"
Class {
	#name : 'AStSmallInteger',
	#superclass : 'AStExactClass',
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'as yet unclassified' }
AStSmallInteger >> couldMergeIn: other [

	| sc oc |
	sc := self class.
	oc := other class.
	^ sc == oc or: [
		  sc == oc superclass ]
]

{ #category : 'initialization' }
AStSmallInteger >> initialize [

	super initialize.
	dataClass := SmallInteger
]
