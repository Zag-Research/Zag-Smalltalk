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

{ #category : 'flow analysis' }
ASParameterNamed >> addFlow: aBasicBlock [

	self location flowValue: ASFlowOnDemand new.
	self next addFlow: aBasicBlock
]

{ #category : 'accessing' }
ASParameterNamed >> countParameters [

	^ 1 + next countParameters
]

{ #category : 'enumerating' }
ASParameterNamed >> dependentValuesDo: ignored [

	"No more dependent values"
]

{ #category : 'data flow instructions' }
ASParameterNamed >> makeStackTheSource [

	location := ASFlowParameter from: location.
	^ next makeStackTheSource
]
