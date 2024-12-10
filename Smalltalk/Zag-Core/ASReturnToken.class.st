Class {
	#name : 'ASReturnToken',
	#superclass : 'ASSyntax',
	#category : 'Zag-Core-Tokens',
	#package : 'Zag-Core',
	#tag : 'Tokens'
}

{ #category : 'parsing' }
ASReturnToken >> statement: anASCompiler [ 
	anASCompiler nextToken. " consume the ^ "
	^ ASReturn new expression: (anASCompiler peek assignExpression: anASCompiler)

]
