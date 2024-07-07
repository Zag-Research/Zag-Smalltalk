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

	source := ASFlowParameter new.
	^ next makeStackTheSource
]
