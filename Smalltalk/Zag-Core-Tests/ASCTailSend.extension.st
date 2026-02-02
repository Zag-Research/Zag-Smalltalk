Extension { #name : 'ASCTailSend' }

{ #category : '*Zag-Core-Tests' }
ASCTailSend >> interpretFor: anInterpreter [
"tail sends don't need contexts, its possible a block may need a context TBD"
	^ anInterpreter
		  beforeTailSend;
		  send: selector
		  "installContextIfNone;
		  resetBeforeCall.
		self halt."
]
