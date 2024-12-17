Class {
	#name : 'ASReturnToken',
	#superclass : 'ASSyntax',
	#category : 'Zag-Compiler-Tokens',
	#package : 'Zag-Compiler',
	#tag : 'Tokens'
}

{ #category : 'parsing' }
ASReturnToken >> statement: anASCompiler [ 
	anASCompiler nextToken. " consume the ^ "
	^ ASReturn new expression: (anASCompiler peek assignExpression: anASCompiler)

]
