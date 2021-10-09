Class {
	#name : #ASAssignmentToken,
	#superclass : #ASSyntax,
	#category : #'ASTSmalltalk-Tokens'
}

{ #category : #parsing }
ASAssignmentToken >> possibleAssign: anASCompiler target: target [
	anASCompiler nextToken.
	^ ASAssignment new target: target value: (anASCompiler peek assignExpression: anASCompiler)
]
