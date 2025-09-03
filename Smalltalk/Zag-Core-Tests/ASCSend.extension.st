Extension { #name : 'ASCSend' }

{ #category : '*Zag-Core-Tests' }
ASCSend >> interpretFor: anInterpreter [

	anInterpreter installContextIfNone.
	^ anInterpreter
		  returnTo: blockToReturnTo;
		  send: selector
]
