Extension { #name : 'ASCSend' }

{ #category : '*Zag-Core-Tests' }
ASCSend >> interpretFor: anInterpreter [
self halt.
	^ anInterpreter
		  send: selector;
		  installContextIfNone: blockToReturnTo
]
