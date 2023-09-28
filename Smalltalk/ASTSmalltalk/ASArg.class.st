"
I represent an argument to a method or a block
"
Class {
	#name : #ASArg,
	#superclass : #ASVariable,
	#category : #'ASTSmalltalk-AST'
}

{ #category : #visiting }
ASArg >> acceptVisitor: aVisitor [ 
	^ aVisitor visitArg: self
]

{ #category : #converting }
ASArg >> asZagCompileObject [
	| results |
	results := OrderedCollection new.
	
	results add: 'c.ASArg,'.
	results add: '":', self asZagLabel ,'",'.
	
^ results
]

{ #category : #visiting }
ASArg >> isASArg [
	^true
]
