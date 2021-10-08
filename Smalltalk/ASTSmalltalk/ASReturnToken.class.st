Class {
	#name : #ASReturnToken,
	#superclass : #ASSyntax,
	#category : #'ASTSmalltalk-Tokens'
}

{ #category : #parsing }
ASReturnToken >> statements: aCollection compiler: anASCompiler [
	anASCompiler nextToken. " consume the ^ "
	aCollection add: (ASReturnNode new expression: (anASCompiler peek assignExpression: anASCompiler)).
	anASCompiler peek nonStatements: anASCompiler.

]
