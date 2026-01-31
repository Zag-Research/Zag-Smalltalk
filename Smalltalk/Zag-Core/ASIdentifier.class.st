Class {
	#name : 'ASIdentifier',
	#superclass : 'ASExpression',
	#instVars : [
		'id'
	],
	#category : 'Zag-Core-AST',
	#package : 'Zag-Core',
	#tag : 'AST'
}

{ #category : 'accessing' }
ASIdentifier >> id [

	^ id 
]

{ #category : 'instance creation' }
ASIdentifier >> with: aString [

	id := aString
]
