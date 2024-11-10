"
I remember local pops
"
Class {
	#name : 'ASCPopVariable',
	#superclass : 'ASCVariableOperations',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'code generation' }
ASCPopVariable >> outputTo: anOutput [

	^ variableLocation popTheVariable: anOutput stackOffset: stack offset
]

{ #category : 'as yet unclassified' }
ASCPopVariable >> shrinksStack [

	^ true
]
