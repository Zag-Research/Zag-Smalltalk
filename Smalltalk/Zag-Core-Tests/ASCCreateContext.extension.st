Extension { #name : 'ASCCreateContext' }

{ #category : '*Zag-Core-Tests' }
ASCCreateContext >> interpretFor: aSemanticInterpreter [

	^ aSemanticInterpreter createContext: method
]
