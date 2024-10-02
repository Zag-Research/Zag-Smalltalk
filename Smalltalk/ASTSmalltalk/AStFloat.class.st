"
I represent a `Float` value, but nothing else is known.

"
Class {
	#name : 'AStFloat',
	#superclass : 'AStExactClass',
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'initialization' }
AStFloat >> initialize [

	super initialize.
	dataClass := Float
]
