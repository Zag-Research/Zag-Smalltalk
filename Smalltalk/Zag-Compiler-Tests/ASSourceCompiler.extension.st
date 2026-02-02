Extension { #name : 'ASSourceCompiler' }

{ #category : '*Zag-Compiler-Tests' }
ASSourceCompiler >> statementForTest: aString [

	aScanner := ASScanner scan: aString.
	^ aScanner peek assignExpression: self
]
