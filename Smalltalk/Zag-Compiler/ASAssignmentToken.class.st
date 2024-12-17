Class {
	#name : 'ASAssignmentToken',
	#superclass : 'ASSyntax',
	#category : 'Zag-Compiler-Tokens',
	#package : 'Zag-Compiler',
	#tag : 'Tokens'
}

{ #category : 'parsing' }
ASAssignmentToken >> possibleAssign: anASCompiler target: target [

	anASCompiler nextToken.
	^ ASAssign new
		  variable: target
		  expression: (anASCompiler peek assignExpression: anASCompiler)
]
