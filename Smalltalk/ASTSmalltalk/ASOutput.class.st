"
I generate runtime compile objects
"
Class {
	#name : 'ASOutput',
	#superclass : 'Object',
	#classTraits : 'ASOutputAPI classTrait',
	#instVars : [
		'context',
		'alreadyGenerated',
		'forClass',
		'ourObject',
		'codeGenerator',
		'parentCodeGenerator',
		'testing',
		'currentBasicBlock',
		'maxDepth',
		'numLocals'
	],
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'compiling' }
ASOutput >> blockClass [
	^ self class blockClass
]

{ #category : 'accessing' }
ASOutput >> blockNumber [
	^ currentBasicBlock := currentBasicBlock + 1.

]

{ #category : 'API' }
ASOutput >> branch: command to: unused [

	^ self subclassResponsibility
]

{ #category : 'code generation' }
ASOutput >> codeGenerator [

	^ codeGenerator ifNil: [
		  codeGenerator := self codeGeneratorClass new
			                   parentCodeGenerator: parentCodeGenerator;
			                   output: self;
			                   targetClass: forClass ]
]

{ #category : 'code generation' }
ASOutput >> codeGeneratorClass [ ^ ASCodeGenerator 
]

{ #category : 'compiling' }
ASOutput >> compileBlock: anASCBlock [
	self assert: anASCBlock block ~= ourObject.
	(self blockClass new testing: testing)
		context: context;
		parentCodeGenerator: codeGenerator;
		methodGenerator: self methodGenerator block: self blockNumber;
		generateCode: anASCBlock block for: forClass
]

{ #category : 'accessing' }
ASOutput >> context: anASContext [

	context := anASContext.
	anASContext addComponent: self
]

{ #category : 'API' }
ASOutput >> endMethod [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
ASOutput >> forClass [

	^ forClass 
]

{ #category : 'code generation' }
ASOutput >> generateCode [

	alreadyGenerated ifTrue: [ ^ self ].
	alreadyGenerated := true.
	self codeGenerator generate: ourObject.
	self outputToStream
]

{ #category : 'generation' }
ASOutput >> generateCode: anASNode [

	context for: anASNode generatorIs: self.
	ourObject := anASNode.
	alreadyGenerated := false.
	context processComponents.
	^ self
]

{ #category : 'generation' }
ASOutput >> generateCode: anASNode for: aClass [

	context for: anASNode generatorIs: self.
	ourObject := anASNode.
	forClass := aClass.
	alreadyGenerated := false.
	context processComponents
]

{ #category : 'initialization' }
ASOutput >> initialize [

	super initialize.
	currentBasicBlock := 0.
	testing := false.
	maxDepth := 0.
	numLocals := 0
]

{ #category : 'code generation' }
ASOutput >> outputToStream [

	codeGenerator finalAnalysis.
	codeGenerator basicBlocksLinearizeDo: [ :bb |
		currentBasicBlock := bb.
		maxDepth := maxDepth max: currentBasicBlock maxDepth.
		currentBasicBlock outputTo: self ].
	self endMethod
]

{ #category : 'accessing' }
ASOutput >> parentCodeGenerator: aCodeGenerator [
	parentCodeGenerator := aCodeGenerator 
]

{ #category : 'API' }
ASOutput >> popLocalData: name stackIndex: stackIndex blockIndex: blockIndex [

	^ self subclassResponsibility
]

{ #category : 'API' }
ASOutput >> pushLocal: localNumber [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
ASOutput >> selector [

	^ codeGenerator selector
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
