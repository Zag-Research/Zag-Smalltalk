Extension { #name : 'ASCSend' }

{ #category : '*Zag-Core-Tests' }
ASCSend >> interpretFor: anInterpreter [

	^ anInterpreter returnTo: blockToReturnTo;send: selector
]
