Extension { #name : 'ASCTailSend' }

{ #category : '*ASTSmalltalk-Tests' }
ASCTailSend >> interpretFor: anInterpreter [

	(inContext and: [ selfLocation isInContext ])
		ifTrue: [
			anInterpreter
				restructureContext: restructure + selfLocation offset ]
		ifFalse: [
			anInterpreter
				restructureStack:
					restructure + (stack offset - selfLocation offset + 1) ].
	^ anInterpreter send: selector
]
