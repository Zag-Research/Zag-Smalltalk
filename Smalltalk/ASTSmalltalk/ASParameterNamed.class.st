"
I am a named parameter element - hence read-only
"
Class {
	#name : 'ASParameterNamed',
	#superclass : 'ASStackNamed',
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'enumerating' }
ASParameterNamed >> dependentValuesDo: ignored [

	"No more dependent values"
]

{ #category : 'data flow instructions' }
ASParameterNamed >> makeStackTheSource [

	data := ASFlowParameter from: data.
	^ next makeStackTheSource
]

{ #category : 'data flow instructions' }
ASParameterNamed >> newElementFor: aBasicBlock [

	| flow |
	flow := aBasicBlock flowForParameters.
	flow ifNil: [ ^ self ].
	^ (self replicate: (next newElementFor: aBasicBlock)) data:
		  (flow new basicBlock: aBasicBlock)
]
