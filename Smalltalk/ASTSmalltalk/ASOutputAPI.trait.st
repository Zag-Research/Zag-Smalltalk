"
I document the API that any outputer needs to be used by ASCodeGenerator
"
Trait {
	#name : 'ASOutputAPI',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'API' }
ASOutputAPI >> branch: command to: unused [
^self explicitRequirement
]

{ #category : 'API' }
ASOutputAPI >> codeGeneratorClass [

	^ self explicitRequirement
]

{ #category : 'API' }
ASOutputAPI >> context: file [

	^ self explicitRequirement
]

{ #category : 'API' }
ASOutputAPI >> endMethod [

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
ASOutputAPI >> popLocalData: name stackIndex: stackIndex blockIndex: blockIndex [

	^ self explicitRequirement
]

{ #category : 'API' }
ASOutputAPI >> pushLocal: localNumber [

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
ASOutputAPI >> visitPushLiteral: anUndefinedObject [

	^ self explicitRequirement
]

{ #category : 'API' }
ASOutputAPI >> visitPushLocal: anUndefinedObject [

	^ self explicitRequirement
]

{ #category : 'API' }
ASOutputAPI >> visitPushLocalOrBlock: anUndefinedObject [

	^ self explicitRequirement
]

{ #category : 'API' }
ASOutputAPI >> visitReturnTop [
	^ self explicitRequirement
]

{ #category : 'API' }
ASOutputAPI >> visitSend: aSelector returnIndex: index target: aClass [


	^ self explicitRequirement
]
