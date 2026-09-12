Extension { #name : 'ASCReturnTop' }

{ #category : '*Zag-Core-Tests' }
ASCReturnTop >> interpretFor: anInterpreter [
self halt.
	^ anInterpreter
		  return: anInterpreter top
		  selfOffset: stack offset + stackElement offset
]
