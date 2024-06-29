"
I remember to push local variable
"
Class {
	#name : 'ASCPushVariable',
	#superclass : 'ASCNonStackReferencingOperations',
	#instVars : [
		'variable'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'code generation' }
ASCPushVariable >> outputTo: anOutput [
	^ variable push: anOutput
]

{ #category : 'stack' }
ASCPushVariable >> resultStack: anASStack [

	^ anASStack push: variable
]

{ #category : 'instance creation' }
ASCPushVariable >> with: target [

	variable := target
]
