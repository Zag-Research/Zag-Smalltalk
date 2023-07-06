"
I represent an instance variable
"
Class {
	#name : #ASInstance,
	#superclass : #ASVariable,
	#category : #'ASTSmalltalk-AST'
}

{ #category : #visiting }
ASInstance >> acceptVisitor: aVisitor [

	^ aVisitor visitInstance: self
]
