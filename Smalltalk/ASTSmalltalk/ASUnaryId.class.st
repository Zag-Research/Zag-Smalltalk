Class {
	#name : #ASUnaryId,
	#superclass : #ASIdentifier,
	#category : #'ASTSmalltalk-Tokens'
}

{ #category : #parsing }
ASUnaryId >> addScope: anASCompiler scope: scope [ 
	anASCompiler nextToken.
	scope add: value.
	anASCompiler peek addScope: anASCompiler scope: scope.

]

{ #category : #accessing }
ASUnaryId >> arity [
	^ 1
]

{ #category : #parsing }
ASUnaryId >> assignExpression: anASCompiler [ 
	anASCompiler nextToken.
	^ anASCompiler peek possibleAssign: anASCompiler target: self
]

{ #category : #parsing }
ASUnaryId >> classSelector: anASCompiler args: aCollection [ 
	^ anASCompiler nextToken value asSymbol
]

{ #category : #parsing }
ASUnaryId >> keywordExpression: anASCompiler target: target [
	anASCompiler nextToken. " consume the selector "
	^ anASCompiler peek keywordExpression: anASCompiler target: (ASSend new target: target selector: self symbol args: #())
]

{ #category : #parsing }
ASUnaryId >> parameterFrom: anASCompiler [ 
	anASCompiler nextToken.
	^ value
]
