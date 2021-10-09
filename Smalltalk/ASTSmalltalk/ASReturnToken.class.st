Class {
	#name : #ASReturnToken,
	#superclass : #ASSyntax,
	#category : #'ASTSmalltalk-Tokens'
}

{ #category : #parsing }
ASReturnToken >> statement: anASCompiler [ 
	anASCompiler nextToken. " consume the ^ "
	^ ASReturnNode new expression: (anASCompiler peek assignExpression: anASCompiler)

]
