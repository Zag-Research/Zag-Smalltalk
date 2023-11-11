"
My subclasses output text versions of methods
"
Class {
	#name : #ASZigMethodOutput,
	#superclass : #ASZigOutput,
	#instVars : [
		'codeGenerator',
		'maxDepth',
		'numLocals',
		'currentBasicBlock'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #compiling }
ASZigMethodOutput >> blockClass [
	^ self class blockClass
]

{ #category : #accessing }
ASZigMethodOutput >> blockNumber [
	^ currentBasicBlock := currentBasicBlock + 1.

]

{ #category : #'code generation' }
ASZigMethodOutput >> codeGenerator [

	^ codeGenerator ifNil: [
		  codeGenerator := ASCodeGenerator new
			                   context: self;
			                   targetClass: forClass ]
]

{ #category : #compiling }
ASZigMethodOutput >> compileBlock: anASCBlock [

	anASCBlock block = ourObject ifTrue: [ ^ self ].
	(self blockClass new testing: testing)
		context: context;
		methodGenerator: self methodGenerator block: self blockNumber;
		generateCode: anASCBlock block
]

{ #category : #'code generation' }
ASZigMethodOutput >> generateCode [

	alreadyGenerated ifTrue: [ ^ self ].
	alreadyGenerated := true.
	self codeGenerator generate: ourObject.
	self outputToStream
]

{ #category : #initialization }
ASZigMethodOutput >> initialize [

	super initialize.
	maxDepth := 0.
	currentBasicBlock := 0
]

{ #category : #compiling }
ASZigMethodOutput >> methodGenerator [ ^ self
]

{ #category : #'code generation' }
ASZigMethodOutput >> outputBasicBlockEnd [

	self subclassResponsibility
]

{ #category : #'code generation' }
ASZigMethodOutput >> outputMethodLabel: bbLabel [ 
	^ self subclassResponsibility
]

{ #category : #'code generation' }
ASZigMethodOutput >> outputMethodStart: bbLabel [

self subclassResponsibility 
]

{ #category : #'code generation' }
ASZigMethodOutput >> outputToStream [ 

self subclassResponsibility 
]

{ #category : #accessing }
ASZigMethodOutput >> prefix [ ^ 'M'
]

{ #category : #accessing }
ASZigMethodOutput >> selector [

	^ codeGenerator selector
]

{ #category : #'code generation' }
ASZigMethodOutput >> verifySelector: anObject [

	self subclassResponsibility 
]

{ #category : #API }
ASZigMethodOutput >> visitDrop [

	self putEmbedded: #drop
]

{ #category : #API }
ASZigMethodOutput >> visitPopLocal: localNumber [ 
	^ self subclassResponsibility
]

{ #category : #API }
ASZigMethodOutput >> visitPushBlock: stackIndex [ 
	^ self subclassResponsibility
]

{ #category : #API }
ASZigMethodOutput >> visitPushContext: unused [

	^ self subclassResponsibility
]

{ #category : #API }
ASZigMethodOutput >> visitPushLiteral: aLiteral [ 
	^ self subclassResponsibility
]

{ #category : #API }
ASZigMethodOutput >> visitPushLocal: localNumber [

	^ self subclassResponsibility
]

{ #category : #'as yet unclassified' }
ASZigMethodOutput >> visitPushLocalOrBlock: anObject [ 
	^ self subclassResponsibility
]

{ #category : #API }
ASZigMethodOutput >> visitReturnTop [
	^ self subclassResponsibility
]

{ #category : #API }
ASZigMethodOutput >> visitSend: aSelector returnIndex: index target: anObject [ 
	^ self subclassResponsibility
]

{ #category : #'as yet unclassified' }
ASZigMethodOutput >> visitStoreLocal: anASLocal [ 
	^ self subclassResponsibility
]

{ #category : #writing }
ASZigMethodOutput >> writeDefinitionOn: anObject [

	self subclassResponsibility
]

{ #category : #writing }
ASZigMethodOutput >> writeInitializationOn: anObject [

	self subclassResponsibility
]
