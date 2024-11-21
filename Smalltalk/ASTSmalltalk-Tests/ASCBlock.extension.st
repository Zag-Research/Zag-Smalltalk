Extension { #name : 'ASCBlock' }

{ #category : '*ASTSmalltalk-Tests' }
ASCBlock >> interpretFor: aSemanticInterpreter [

	^ aSemanticInterpreter push: block compiledUnit
]
