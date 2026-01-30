Extension { #name : 'ZagSmalltalk' }

{ #category : '*Zag-Compiler-Tests' }
ZagSmalltalk >> statementForTest: aString [

	aScanner := ZagSmalltalkScanner scan: aString.
	^ aScanner peek assignExpression: self
]
