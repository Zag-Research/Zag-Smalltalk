Extension { #name : 'ASSourceCompiler' }

{ #category : '*Zag-Core-Tests' }
ASSourceCompiler >> statementForTest: aString [
	aScanner := ASScanner new scanCode: aString readStream.
	^ aScanner peek assignExpression: self
]
