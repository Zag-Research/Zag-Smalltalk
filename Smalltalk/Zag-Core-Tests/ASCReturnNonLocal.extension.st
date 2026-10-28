Extension { #name : 'ASCReturnNonLocal' }

{ #category : '*Zag-Core-Tests' }
ASCReturnNonLocal >> interpretFor: anInterpreter [ 

"need to adjust stack by some offset"
"need to return to defining blocks method, or terminate"

"(inContext and: [ stackElement isInContext ]) ifTrue: [
		^ anInterpreter
			  return: anInterpreter top
			  selfContext: stackElement offset ]."
	^ anInterpreter
		  returnNonLocal: anInterpreter top
]
