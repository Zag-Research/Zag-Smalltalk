Class {
	#name : 'ASSimple',
	#superclass : 'ASToken',
	#category : 'ASTSmalltalk-Tokens',
	#package : 'ASTSmalltalk',
	#tag : 'Tokens'
}

{ #category : 'parsing' }
ASSimple >> assignExpression: anASCompiler [ 
	^ self keywordExpression: anASCompiler
]

{ #category : 'parsing' }
ASSimple >> binaryExpression: anASCompiler [

	anASCompiler nextToken.
	^ anASCompiler peek
		  binaryExpression: anASCompiler
		  target: (self parseWith: anASCompiler)
]

{ #category : 'parsing' }
ASSimple >> keywordExpression: anASCompiler [

	anASCompiler nextToken.
	^ anASCompiler peek
		  keywordExpression: anASCompiler
		  target: (self parseWith: anASCompiler)
]

{ #category : 'parsing' }
ASSimple >> parseWith: anASCompiler [

]

{ #category : 'parsing' }
ASSimple >> unaryExpression: anASCompiler [

	anASCompiler nextToken.
	^ anASCompiler peek
		  unaryExpression: anASCompiler
		  target: (self parseWith: anASCompiler)
]
