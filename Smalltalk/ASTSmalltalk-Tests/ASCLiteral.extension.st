Extension { #name : 'ASCLiteral' }

{ #category : '*ASTSmalltalk-Tests' }
ASCLiteral >> interpretFor: anInterpreter [

	^ anInterpreter push: literal
]
