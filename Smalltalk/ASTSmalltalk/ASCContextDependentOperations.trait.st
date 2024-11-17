Trait {
	#name : 'ASCContextDependentOperations',
	#instVars : [
		'inContext'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'initialization' }
ASCContextDependentOperations >> initialize [

	super initialize.
	inContext := false
]

{ #category : 'initialization' }
ASCContextDependentOperations >> setInContext [

	inContext := true
]

{ #category : 'initialization' }
ASCContextDependentOperations >> setInContext: aBoolean [

	inContext := aBoolean
]
