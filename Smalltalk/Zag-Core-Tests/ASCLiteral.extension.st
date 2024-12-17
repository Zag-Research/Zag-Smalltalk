Extension { #name : 'ASCLiteral' }

{ #category : '*Zag-Core-Tests' }
ASCLiteral >> interpretFor: anInterpreter [

	^ anInterpreter push: literal
]
