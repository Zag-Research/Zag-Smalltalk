Class {
	#name : #ASReturnToken,
	#superclass : #ASSyntax,
	#category : #'ASTSmalltalk-Tokens'
}

{ #category : #parsing }
ASReturnToken >> statement: anASCompiler [ 
	anASCompiler nextToken. " consume the ^ "
	^ ASReturn new expression: (anASCompiler peek parseWith: anASCompiler)

]
