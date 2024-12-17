Extension { #name : 'ASSourceCompiler' }

{ #category : '*Zag-Compiler-Tests' }
ASSourceCompiler >> statementForTest: aString [
	aScanner := ASScanner new scanCode: aString readStream.
	^ aScanner peek assignExpression: self
]
