Extension { #name : 'ASCSend' }

{ #category : '*Zag-Core-Tests' }
ASCSend >> interpretFor: anInterpreter [
self halt.
	^ anInterpreter
		  returnTo: blockToReturnTo;
		  send: selector
]
