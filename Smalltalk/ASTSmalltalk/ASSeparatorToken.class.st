Class {
	#name : #ASSeparatorToken,
	#superclass : #ASSyntax,
	#category : #'ASTSmalltalk-Tokens'
}

{ #category : #parsing }
ASSeparatorToken >> keywordExpression: anASCompiler target: target [
	^ target
]

{ #category : #parsing }
ASSeparatorToken >> nonStatements: anASCompiler [ 
	anASCompiler nextToken. " consume the . "
	anASCompiler peek nonStatements: anASCompiler
]

{ #category : #parsing }
ASSeparatorToken >> statements: anASCompiler [ 
	self shouldBeImplemented.
]

{ #category : #parsing }
ASSeparatorToken >> statements: aCollection compiler: anASCompiler [ 
	anASCompiler nextToken. " consume the . "
	^ anASCompiler peek statements: aCollection compiler: anASCompiler
]
