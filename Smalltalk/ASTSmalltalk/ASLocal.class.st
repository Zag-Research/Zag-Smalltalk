"
I represent a local (temporary) variable
"
Class {
	#name : #ASLocal,
	#superclass : #ASVariable,
	#category : #'ASTSmalltalk-AST'
}

{ #category : #visiting }
ASLocal >> acceptVisitor: aVisitor [
	^ aVisitor visitNamedAS: self
]

{ #category : #converting }
ASLocal >> asZagCompileObject [

	| results |
	results := OrderedCollection new.

	results add: 'c.ASLocal,'.
	results add: '":' , self asZagLabel , '",'.
	results add: name.

	^ results
]
