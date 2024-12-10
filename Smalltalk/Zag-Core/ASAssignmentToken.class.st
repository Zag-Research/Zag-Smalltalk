Class {
	#name : 'ASAssignmentToken',
	#superclass : 'ASSyntax',
	#category : 'Zag-Core-Tokens',
	#package : 'Zag-Core',
	#tag : 'Tokens'
}

{ #category : 'parsing' }
ASAssignmentToken >> possibleAssign: anASCompiler target: target [

	anASCompiler nextToken.
	^ ASAssign new
		  variable: target
		  expression: (anASCompiler peek assignExpression: anASCompiler)
]
