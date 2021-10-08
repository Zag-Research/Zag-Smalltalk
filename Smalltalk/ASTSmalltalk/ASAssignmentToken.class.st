Class {
	#name : #ASAssignmentToken,
	#superclass : #ASSyntax,
	#category : #'ASTSmalltalk-Tokens'
}

{ #category : #parsing }
ASAssignmentToken >> possibleAssign: anASCompiler target: anASUnaryId [
	anASCompiler nextToken.
	^ ASAssignment new target: anASUnaryId value: (anASCompiler peek expression: anASCompiler)
]
