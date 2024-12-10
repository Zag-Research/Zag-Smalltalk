Class {
	#name : 'ASSeparatorToken',
	#superclass : 'ASClosers',
	#category : 'Zag-Core-Tokens',
	#package : 'Zag-Core',
	#tag : 'Tokens'
}

{ #category : 'parsing' }
ASSeparatorToken >> nonStatements: anASCompiler [ 
	anASCompiler nextToken. " consume the . "
	anASCompiler peek nonStatements: anASCompiler
]

{ #category : 'parsing' }
ASSeparatorToken >> statement: anASCompiler [ 
	anASCompiler nextToken. " consume the . "
	^ anASCompiler peek statement: anASCompiler
]
