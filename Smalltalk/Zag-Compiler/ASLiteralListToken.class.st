Class {
	#name : 'ASLiteralListToken',
	#superclass : 'ASSimple',
	#category : 'Zag-Compiler-Tokens',
	#package : 'Zag-Compiler',
	#tag : 'Tokens'
}

{ #category : 'parsing' }
ASLiteralListToken >> keywordExpression: anASCompiler [

	anASCompiler nextToken.
	^ anASCompiler peek
		  literalExpressionList: anASCompiler
		  into: OrderedCollection new
]

{ #category : 'parsing' }
ASLiteralListToken >> parseWith: anASCompiler [

]
