"
I represent a class variable
"
Class {
	#name : #ASClassVar,
	#superclass : #ASVariable,
	#category : #'ASTSmalltalk-AST'
}

{ #category : #visiting }
ASClassVar >> acceptVisitor: aVisitor [ 
	^aVisitor visitClassVar: self
]
