Extension { #name : 'ASCTailSend' }

{ #category : '*Zag-Core-Tests' }
ASCTailSend >> interpretFor: anInterpreter [
self halt.
	^ anInterpreter
		  send: selector;
		  installContextIfNone;
		  resetBeforeCall
]
