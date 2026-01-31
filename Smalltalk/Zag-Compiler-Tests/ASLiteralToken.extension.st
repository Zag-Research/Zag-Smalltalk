Extension { #name : 'ASLiteralToken' }

{ #category : '*Zag-Compiler-Tests' }
ASLiteralToken >> asAST [

	^ ASLiteral with: value
]

{ #category : '*Zag-Compiler-Tests' }
ASLiteralToken >> baseValue [
	^ value
]
