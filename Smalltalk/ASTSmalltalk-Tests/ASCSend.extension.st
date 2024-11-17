Extension { #name : 'ASCSend' }

{ #category : '*ASTSmalltalk-Tests' }
ASCSend >> interpretFor: anInterpreter [

	^ anInterpreter returnTo: blockToReturnTo;send: selector
]
