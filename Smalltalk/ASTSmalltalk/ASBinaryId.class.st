Class {
	#name : #ASBinaryId,
	#superclass : #ASIdentifier,
	#category : #'ASTSmalltalk-Tokens'
}

{ #category : #accessing }
ASBinaryId >> arity [
	^ 2
]

{ #category : #parsing }
ASBinaryId >> binaryExpression: anASCompiler target: target [
	| arg |
	anASCompiler nextToken. " consume the selector "
	arg := anASCompiler peek unaryExpression: anASCompiler.
	^ anASCompiler peek binaryExpression: anASCompiler target: (ASSend new target: target selector: value args: { arg })
]

{ #category : #parsing }
ASBinaryId >> classSelector: anASCompiler args: args [ 
	anASCompiler nextToken.
	args add: (anASCompiler peek parameterFrom: anASCompiler).
	^ value
]

{ #category : #parsing }
ASBinaryId >> keywordExpression: anASCompiler target: target [ 
	| arg |
	anASCompiler nextToken. " consume the selector "
	arg := anASCompiler peek unaryExpression: anASCompiler.
	^ anASCompiler peek keywordExpression: anASCompiler target: (ASSend new target: target selector: value args: { arg })
]
