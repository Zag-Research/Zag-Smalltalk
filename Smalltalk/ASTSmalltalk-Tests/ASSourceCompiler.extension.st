Extension { #name : #ASSourceCompiler }

{ #category : #'*ASTSmalltalk-Tests' }
ASSourceCompiler >> statementForTest: aString [
	aScanner := ASScanner new scanCode: aString readStream.
	"^ aScanner peek kewordExpression: self"
	^ aScanner peek parseWith: self
]
