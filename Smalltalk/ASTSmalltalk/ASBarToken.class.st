Class {
	#name : #ASBarToken,
	#superclass : #ASBinaryId,
	#category : 'ASTSmalltalk-Tokens'
}

{ #category : #parsing }
ASBarToken >> addScope: anASCompiler scope: scope [ 
	anASCompiler nextToken.

]

{ #category : #parsing }
ASBarToken >> defs: anASCompiler scope: scope [ 
	anASCompiler nextToken.
	anASCompiler peek addScope: anASCompiler scope: scope.

]
