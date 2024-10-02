Class {
	#name : 'ASReturnToken',
	#superclass : 'ASSyntax',
	#category : 'ASTSmalltalk-Tokens',
	#package : 'ASTSmalltalk',
	#tag : 'Tokens'
}

{ #category : 'parsing' }
ASReturnToken >> statement: anASCompiler [ 
	anASCompiler nextToken. " consume the ^ "
	^ ASReturn new expression: (anASCompiler peek assignExpression: anASCompiler)

]
