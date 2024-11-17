Extension { #name : 'ASCArray' }

{ #category : '*ASTSmalltalk-Tests' }
ASCArray >> interpretFor: aSemanticInterpreter [

	| result |
	result := Array new: size.
	0 to: size - 1 do: [ :offset |
	result at: size - offset put: aSemanticInterpreter pop ].
	^ aSemanticInterpreter push: result
]
