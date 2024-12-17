"
I remember local pops
"
Class {
	#name : 'ASCPopVariable',
	#superclass : 'ASCVariableOperations',
	#category : 'Zag-Core-Operations',
	#package : 'Zag-Core',
	#tag : 'Operations'
}

{ #category : 'code generation' }
ASCPopVariable >> outputTo: anOutput [

	^ variableLocation popTheVariable: anOutput stackOffset: stack offset
]

{ #category : 'stack' }
ASCPopVariable >> resultStack: anASStack basicBlock: aBasicBlock [

	stack := anASStack.
	^ anASStack drop
]

{ #category : 'as yet unclassified' }
ASCPopVariable >> shrinksStack [

	^ true
]
