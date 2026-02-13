"
I represent some kind of ""variable"" (even though some may be immutable).

I am referenced by AST elements:
 - `ASClassNode` for instance and class variables
 - `ASCodeBlock` for locals and parameters
 - `ASRef` to access (push) a variable
 - `ASAssign` to store (pop) a value to a variable
"
Class {
	#name : 'ASVariable',
	#superclass : 'Object',
	#instVars : [
		'id'
	],
	#category : 'Zag-Core-AST',
	#package : 'Zag-Core',
	#tag : 'AST'
}

{ #category : 'accessing' }
ASVariable >> id [

	^ id 
]

{ #category : 'instance creation' }
ASVariable >> with: aString [

	id := aString
]
