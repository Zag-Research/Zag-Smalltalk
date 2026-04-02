Extension { #name : 'ASCSend' }

{ #category : '*Zag-Core-Tests' }
ASCSend >> interpretFor: anInterpreter [

	^ anInterpreter
		  send: selector;
		  installContextIfNone: blockToReturnTo
]
