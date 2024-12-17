"
I represent a `Float` value, but nothing else is known.

"
Class {
	#name : 'AStUndefinedClass',
	#superclass : 'AStExactClass',
	#category : 'Zag-Core-Stack',
	#package : 'Zag-Core',
	#tag : 'Stack'
}

{ #category : 'initialization' }
AStUndefinedClass >> initialize [

	super initialize.
	dataClass := UndefinedClass
]
