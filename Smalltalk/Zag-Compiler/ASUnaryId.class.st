Class {
	#name : 'ASUnaryId',
	#superclass : 'ASIdentifier',
	#category : 'Zag-Compiler-Tokens',
	#package : 'Zag-Compiler',
	#tag : 'Tokens'
}

{ #category : 'parsing' }
ASUnaryId >> addScope: anASCompiler scope: scope [ 
	anASCompiler nextToken.
	scope add: value.
	anASCompiler peek addScope: anASCompiler scope: scope.

]

{ #category : 'accessing' }
ASUnaryId >> arity [
	^ 1
]

{ #category : 'parsing' }
ASUnaryId >> assignExpression: anASCompiler [
	anASCompiler nextToken.
	^ anASCompiler peek possibleAssign: anASCompiler target: self
]

{ #category : 'parsing' }
ASUnaryId >> binaryExpression: anASCompiler target: target [

	anASCompiler nextToken. " consume the selector "
	^ anASCompiler peek
		  binaryExpression: anASCompiler
		  target: (ASSend new target: target; selector: value args: #(  ))
]

{ #category : 'parsing' }
ASUnaryId >> classSelector: anASCompiler args: aCollection [ 
	^ anASCompiler nextToken value asSymbol
]

{ #category : 'parsing' }
ASUnaryId >> keywordExpression: anASCompiler target: target [

	anASCompiler nextToken. " consume the selector "
	^ anASCompiler peek
		  keywordExpression: anASCompiler
		  target: (ASSend new target: target; selector: value args: #(  ))
]

{ #category : 'parsing' }
ASUnaryId >> parameterFrom: anASCompiler [ 
	anASCompiler nextToken.
	^ value
]

{ #category : 'parsing' }
ASUnaryId >> statement: anASCompiler [
	^ self assignExpression: anASCompiler 

]

{ #category : 'parsing' }
ASUnaryId >> unaryExpression: anASCompiler target: target [

	anASCompiler nextToken. " consume the selector "
	^ anASCompiler peek
		  unaryExpression: anASCompiler
		  target:
		  (ASSend new target: target; selector: self symbol args: #(  ))
]
