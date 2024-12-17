"
I represent a `Float` value, but nothing else is known.

"
Class {
	#name : 'AStFloat',
	#superclass : 'AStExactClass',
	#category : 'Zag-Core-Stack',
	#package : 'Zag-Core',
	#tag : 'Stack'
}

{ #category : 'initialization' }
AStFloat >> initialize [

	super initialize.
	dataClass := Float
]
