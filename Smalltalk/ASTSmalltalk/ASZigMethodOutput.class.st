"
My subclasses output text versions of methods
"
Class {
	#name : 'ASZigMethodOutput',
	#superclass : 'ASZigOutput',
	#classTraits : '{} + TraitedClass',
	#instVars : [
		'codeGenerator',
		'maxDepth',
		'numLocals',
		'currentBasicBlock',
		'parentCodeGenerator'
	],
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'compiling' }
ASZigMethodOutput >> blockClass [
	^ self class blockClass
]

{ #category : 'accessing' }
ASZigMethodOutput >> blockNumber [
	^ currentBasicBlock := currentBasicBlock + 1.

]

{ #category : 'code generation' }
ASZigMethodOutput >> codeGenerator [

	^ codeGenerator ifNil: [
		  codeGenerator := self codeGeneratorClass new
			                   parentCodeGenerator: parentCodeGenerator;
			                   output: self;
			                   targetClass: forClass ]
]

{ #category : 'code generation' }
ASZigMethodOutput >> codeGeneratorClass [ ^ ASCodeGenerator 
]

{ #category : 'compiling' }
ASZigMethodOutput >> compileBlock: anASCBlock [
	self assert: anASCBlock block ~= ourObject.
	(self blockClass new testing: testing)
		context: context;
		parentCodeGenerator: codeGenerator;
		methodGenerator: self methodGenerator block: self blockNumber;
		generateCode: anASCBlock block for: forClass
]

{ #category : 'code generation' }
ASZigMethodOutput >> generateCode [

	alreadyGenerated ifTrue: [ ^ self ].
	alreadyGenerated := true.
	self codeGenerator generate: ourObject.
	self outputToStream
]

{ #category : 'initialization' }
ASZigMethodOutput >> initialize [

	super initialize.
	maxDepth := 0.
	currentBasicBlock := 0
]

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

{ #category : 'code generation' }
ASZigMethodOutput >> outputToStream [ 

self subclassResponsibility 
]

{ #category : 'accessing' }
ASZigMethodOutput >> parentCodeGenerator: aCodeGenerator [
	parentCodeGenerator := aCodeGenerator 
]

{ #category : 'accessing' }
ASZigMethodOutput >> prefix [ ^ 'M'
]

{ #category : 'accessing' }
ASZigMethodOutput >> selector [

	^ codeGenerator selector
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
