Class {
	#name : 'ASVariable',
	#superclass : 'ASExpression',
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
