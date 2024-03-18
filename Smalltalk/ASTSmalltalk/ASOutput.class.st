"
I generate runtime compile objects
"
Class {
	#name : 'ASOutput',
	#superclass : 'Object',
	#classTraits : 'ASOutputAPI classTrait',
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'API' }
ASOutput >> branch: command to: unused [

	^ self subclassResponsibility
]

{ #category : 'API' }
ASOutput >> codeGeneratorClass [

	^ self subclassResponsibility
]

{ #category : 'API' }
ASOutput >> context: file [

	^ self subclassResponsibility
]

{ #category : 'API' }
ASOutput >> endMethod [

	^ self subclassResponsibility
]

{ #category : 'API' }
ASOutput >> generateCode [

	^ self subclassResponsibility
]

{ #category : 'API' }
ASOutput >> generateCode: unused [

	^ self subclassResponsibility
]

{ #category : 'API' }
ASOutput >> popLocalData: name stackIndex: stackIndex blockIndex: blockIndex [

	^ self subclassResponsibility
]

{ #category : 'API' }
ASOutput >> pushLocal: localNumber [

	^ self subclassResponsibility
]

{ #category : 'API' }
ASOutput >> visitPopLocal: anASLocal [

	^ self subclassResponsibility
]

{ #category : 'API' }
ASOutput >> visitPushBlock: anASCBlock [

	^ self subclassResponsibility
]

{ #category : 'API' }
ASOutput >> visitPushContext: anUndefinedObject [

	^ self subclassResponsibility
]

{ #category : 'API' }
ASOutput >> visitPushLiteral: anUndefinedObject [

	^ self subclassResponsibility
]

{ #category : 'API' }
ASOutput >> visitPushLocal: anUndefinedObject [

	^ self subclassResponsibility
]

{ #category : 'API' }
ASOutput >> visitPushLocalOrBlock: anUndefinedObject [

	^ self subclassResponsibility
]

{ #category : 'API' }
ASOutput >> visitReturnTop [

	^ self subclassResponsibility
]

{ #category : 'API' }
ASOutput >> visitSend: aSelector returnIndex: index target: aClass [

	^ self subclassResponsibility
]
