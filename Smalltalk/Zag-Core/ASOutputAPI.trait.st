"
I document the API that any outputer needs to be used by ASCodeGenerator
"
Trait {
	#name : 'ASOutputAPI',
	#category : 'Zag-Core-Output',
	#package : 'Zag-Core',
	#tag : 'Output'
}

{ #category : 'API' }
ASOutputAPI >> codeGeneratorClass [

	^ self explicitRequirement
]

{ #category : 'API' }
ASOutputAPI >> context: file [

	^ self explicitRequirement
]

{ #category : 'API' }
ASOutputAPI >> generateCode [
	^ self explicitRequirement
]

{ #category : 'API' }
ASOutputAPI >> generateCode: unused [

	^ self explicitRequirement
]

{ #category : 'API' }
ASOutputAPI >> outputMethodEnd [

	^ self explicitRequirement
]

{ #category : 'API' }
ASOutputAPI >> visitCase: aClassToBlockMap [


	^ self explicitRequirement
]

{ #category : 'API' }
ASOutputAPI >> visitPopLocal: anASLocal [


	^ self explicitRequirement
]

{ #category : 'API' }
ASOutputAPI >> visitPushBlock: anASCBlock [


	^ self explicitRequirement
]

{ #category : 'API' }
ASOutputAPI >> visitPushContext [


	^ self explicitRequirement
]

{ #category : 'API' }
ASOutputAPI >> visitPushContext: anUndefinedObject [


	^ self explicitRequirement
]

{ #category : 'API' }
ASOutputAPI >> visitPushFromStack: anInteger [

	^ self explicitRequirement
]

{ #category : 'API' }
ASOutputAPI >> visitPushLiteral: anUndefinedObject [

	^ self explicitRequirement
]

{ #category : 'API' }
ASOutputAPI >> visitPushLocalOrBlock: anUndefinedObject [

	^ self explicitRequirement
]

{ #category : 'API' }
ASOutputAPI >> visitReturnNil: anOperation [

	^ self explicitRequirement
]

{ #category : 'API' }
ASOutputAPI >> visitReturnSelf: anOperation [

	^ self explicitRequirement
]

{ #category : 'API' }
ASOutputAPI >> visitReturnTop: anOperation [

	^ self explicitRequirement
]

{ #category : 'API' }
ASOutputAPI >> visitSend: aSelector returnIndex: index receiver: aClass [


	^ self explicitRequirement
]

{ #category : 'API' }
ASOutputAPI >> visitTailSend: selector receiver: receiver [

	^ self explicitRequirement
]
