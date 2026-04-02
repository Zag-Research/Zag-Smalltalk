Extension { #name : 'ASCBranch' }

{ #category : '*Zag-Core-Tests' }
ASCBranch >> interpretFor: anInterpreter [
	bb ifNotNil: [^ anInterpreter branch: bb].
	^ anInterpreter
		  return: anInterpreter top
		  selfOffset: stack offset
]
