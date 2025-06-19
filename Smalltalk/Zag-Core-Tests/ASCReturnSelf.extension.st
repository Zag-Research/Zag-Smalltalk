Extension { #name : 'ASCReturnSelf' }

{ #category : '*Zag-Core-Tests' }
ASCReturnSelf >> interpretFor: anInterpreter [

	inContext ifTrue: [
			^ anInterpreter
				  return: anInterpreter top
				  selfContext: anInterpreter selfObject ].
	^ anInterpreter
		  return: anInterpreter top
		  selfOffset: stack offset
]
