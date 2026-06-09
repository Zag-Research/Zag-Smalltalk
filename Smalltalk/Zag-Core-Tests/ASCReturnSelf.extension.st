Extension { #name : 'ASCReturnSelf' }

{ #category : '*Zag-Core-Tests' }
ASCReturnSelf >> interpretFor: anInterpreter [
| value |
	value := anInterpreter
		return: anInterpreter self
		selfOffset: 10.
	anInterpreter dropContext.
	^ value
]
