"
My subclasses output text versions of methods
"
Class {
	#name : 'ASZigMethodOutput',
	#superclass : 'ASZigOutput',
	#classTraits : '{} + TraitedClass',
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'compiling' }
ASZigMethodOutput >> methodGenerator [ ^ self
]

{ #category : 'code generation' }
ASZigMethodOutput >> outputBasicBlockEnd [

	self subclassResponsibility
]

{ #category : 'code generation' }
ASZigMethodOutput >> outputMethodLabel: bbLabel [ 
	^ self subclassResponsibility
]

{ #category : 'code generation' }
ASZigMethodOutput >> outputMethodStart: bbLabel [

self subclassResponsibility 
]

{ #category : 'accessing' }
ASZigMethodOutput >> prefix [ ^ 'M'
]

{ #category : 'settings' }
ASZigMethodOutput >> tailCallOptimize: aBoolean [ 
	^ self codeGenerator tailCallOptimize: aBoolean
]

{ #category : 'code generation' }
ASZigMethodOutput >> verifySelector: anObject [

	self subclassResponsibility 
]

{ #category : 'API' }
ASZigMethodOutput >> visitPopLocal: localNumber [ 
	^ self subclassResponsibility
]

{ #category : 'API' }
ASZigMethodOutput >> visitPushBlock: stackIndex [ 
	^ self subclassResponsibility
]

{ #category : 'API' }
ASZigMethodOutput >> visitPushContext: unused [

	^ self subclassResponsibility
]

{ #category : 'API' }
ASZigMethodOutput >> visitPushLiteral: aLiteral [ 
	^ self subclassResponsibility
]

{ #category : 'API' }
ASZigMethodOutput >> visitPushLocal: localNumber [

	^ self subclassResponsibility
]

{ #category : 'API' }
ASZigMethodOutput >> visitReturnTop [
	^ self subclassResponsibility
]

{ #category : 'API' }
ASZigMethodOutput >> visitSend: aSelector returnIndex: index target: anObject [ 
	^ self subclassResponsibility
]

{ #category : 'visiting' }
ASZigMethodOutput >> visitSimple: aSymbol [ 
	^ self putEmbedded: aSymbol
]

{ #category : 'writing' }
ASZigMethodOutput >> writeDefinitionOn: anObject [

	self subclassResponsibility
]

{ #category : 'writing' }
ASZigMethodOutput >> writeInitializationOn: anObject [

	self subclassResponsibility
]
