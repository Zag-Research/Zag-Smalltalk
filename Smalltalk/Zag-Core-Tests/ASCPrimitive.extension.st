Extension { #name : 'ASCPrimitive' }

{ #category : '*Zag-Core-Tests' }
ASCPrimitive >> interpretFor: anInterpreter [

	| selector |
	selector := Primitives at: primitive.
	selector numArgs == 0 ifTrue: [
		^ anInterpreter unary: [ :selfArg | selfArg perform: selector ] ].
	selector numArgs == 1 ifTrue: [
		^ anInterpreter binary: [ :selfArg :other |
			  selfArg perform: selector with: other ] ].
	selector numArgs == 2 ifTrue: [
		^ anInterpreter ternary: [ :selfArg :other :other2 |
			  selfArg perform: selector with: other with: other2 ] ]
]
