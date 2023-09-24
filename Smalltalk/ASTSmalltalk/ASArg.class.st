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
	results add: '":', name,'",'.
	
^ results
]

{ #category : #converting }
ASArg >> asZagLabel [
	^name
]

{ #category : #visiting }
ASArg >> isASArg [
	^true
]
