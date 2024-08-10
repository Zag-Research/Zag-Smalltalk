"
I represent a `Float` value, but nothing else is known.

"
Class {
	#name : 'AStUndefinedClass',
	#superclass : 'AStExactClass',
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'initialization' }
AStUndefinedClass >> initialize [

	super initialize.
	dataClass := UndefinedClass
]
