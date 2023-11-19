"
I represent a class variable
"
Class {
	#name : 'ASClassVariable',
	#superclass : 'ASVariable',
	#category : 'ASTSmalltalk-AST',
	#package : 'ASTSmalltalk',
	#tag : 'AST'
}

{ #category : 'visiting' }
ASClassVariable >> acceptVisitor: aVisitor [ 
	^aVisitor visitClassVar: self
]
