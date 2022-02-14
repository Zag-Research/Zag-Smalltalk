Extension { #name : #ASCompiler }

{ #category : #'*ASTSmalltalk-Tests' }
ASCompiler >> statementForTest: aString [
	aScanner := ASScanner new scanCode: aString readStream.
	^ aScanner peek assignExpression: self
]
