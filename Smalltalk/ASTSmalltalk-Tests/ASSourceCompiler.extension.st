Extension { #name : 'ASSourceCompiler' }

{ #category : '*ASTSmalltalk-Tests' }
ASSourceCompiler >> statementForTest: aString [
	aScanner := ASScanner new scanCode: aString readStream.
	^ aScanner peek assignExpression: self
]
