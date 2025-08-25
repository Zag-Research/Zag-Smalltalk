Trait {
	#name : 'ASCContextDependentOperations',
	#instVars : [
		'inContext'
	],
	#category : 'Zag-Core-Operations',
	#package : 'Zag-Core',
	#tag : 'Operations'
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
