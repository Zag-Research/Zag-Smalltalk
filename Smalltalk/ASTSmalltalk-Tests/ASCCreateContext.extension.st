Extension { #name : 'ASCCreateContext' }

{ #category : '*ASTSmalltalk-Tests' }
ASCCreateContext >> interpretFor: aSemanticInterpreter [

	^ aSemanticInterpreter createContext
]
