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

{ #category : #visiting }
ASArg >> isASArg [
	^true
]
