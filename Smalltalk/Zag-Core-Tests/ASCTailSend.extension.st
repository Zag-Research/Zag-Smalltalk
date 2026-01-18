Extension { #name : 'ASCTailSend' }

{ #category : '*Zag-Core-Tests' }
ASCTailSend >> interpretFor: anInterpreter [

	^ anInterpreter
		  send: selector;
		  installContextIfNone;
		  resetBeforeCall
]
