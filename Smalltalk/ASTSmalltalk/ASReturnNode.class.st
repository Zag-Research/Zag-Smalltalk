Class {
	#name : #ASReturnNode,
	#superclass : #Object,
	#instVars : [
		'expression'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #accessing }
ASReturnNode >> expression: anASNode [
	expression := anASNode
]

{ #category : #testing }
ASReturnNode >> isReturn [
	^ true
]
