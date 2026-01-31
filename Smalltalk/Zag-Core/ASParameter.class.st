Class {
	#name : 'ASParameter',
	#superclass : 'ASIdentifier',
	#category : 'Zag-Core-AST',
	#package : 'Zag-Core',
	#tag : 'AST'
}

{ #category : 'accessing' }
ASParameter >> accept: aVisitor [

	^ aVisitor visitParameter: self
]
