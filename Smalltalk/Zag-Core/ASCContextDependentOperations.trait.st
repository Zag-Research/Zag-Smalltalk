Trait {
	#name : 'ASCContextDependentOperations',
	#instVars : [
		'inContext'
	],
	#category : 'Zag-Core-CodeGen',
	#package : 'Zag-Core',
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
