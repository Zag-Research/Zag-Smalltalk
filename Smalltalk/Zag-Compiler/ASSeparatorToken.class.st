Class {
	#name : 'ASSeparatorToken',
	#superclass : 'ASClosers',
	#category : 'Zag-Compiler-Tokens',
	#package : 'Zag-Compiler',
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
