Extension { #name : 'ASCPrimitive2' }

{ #category : '*ASTSmalltalk-Tests' }
ASCPrimitive2 >> interpretFor: anInterpreter [

	^ anInterpreter binary: [ :selfArg :other :other2|
		  selfArg perform: primitive with: other with: other2]
]
