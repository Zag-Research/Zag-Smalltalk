"
I generate runtime compile objects
"
Class {
	#name : 'ASOutput',
	#superclass : 'Object',
	#traits : 'ASOutputAPI',
	#classTraits : 'ASOutputAPI classTrait',
	#instVars : [
		'alreadyGenerated',
		'ourObject',
		'codeGenerator',
		'parentCodeGenerator',
		'forClass',
		'currentBasicBlock',
		'maxDepth',
		'context',
		'testing',
		'numLocals'
	],
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'API' }
ASOutput >> blockClass [
	^ self class blockClass
]

{ #category : 'API' }
ASOutput >> blockNumber [
	^ currentBasicBlock := currentBasicBlock + 1
]

{ #category : 'API' }
ASOutput >> codeGenerator [

	^ codeGenerator ifNil: [
		  codeGenerator := self codeGeneratorClass new
			                   parentCodeGenerator: parentCodeGenerator;
			                   output: self;
			                   targetClass: forClass ]
]

{ #category : 'API' }
ASOutput >> codeGeneratorClass [ ^ ASCodeGenerator
]

{ #category : 'API' }
ASOutput >> compileBlock: anASCBlock [
	self assert: anASCBlock block ~= ourObject.
	(self blockClass new testing: testing)
		context: context;
		parentCodeGenerator: codeGenerator;
		methodGenerator: self methodGenerator block: self blockNumber;
		generateCode: anASCBlock block for: forClass
]

{ #category : 'API' }
ASOutput >> context: anASContext [

	context := anASContext.
	anASContext addComponent: self
]

{ #category : 'API' }
ASOutput >> forClass [

	^ forClass
]

{ #category : 'API' }
ASOutput >> generateCode [

	alreadyGenerated ifTrue: [ ^ self ].
	alreadyGenerated := true.
	self codeGenerator generate: ourObject.
	self outputToStream
]

{ #category : 'API' }
ASOutput >> generateCode: anASNode [

	context for: anASNode generatorIs: self.
	ourObject := anASNode.
	alreadyGenerated := false.
	context processComponents.
	^ self
]

{ #category : 'API' }
ASOutput >> generateCode: anASNode for: aClass [

	context for: anASNode generatorIs: self.
	ourObject := anASNode.
	forClass := aClass.
	alreadyGenerated := false.
	context processComponents
]

{ #category : 'API' }
ASOutput >> initialize [

	super initialize.
	currentBasicBlock := 0.
	testing := false.
	maxDepth := 0.
	numLocals := 0
]

{ #category : 'API' }
ASOutput >> outputToStream [

	codeGenerator finalAnalysis.
	codeGenerator basicBlocksLinearizeDo: [ :bb |
		currentBasicBlock := bb.
		maxDepth := maxDepth max: currentBasicBlock maxDepth.
		currentBasicBlock outputTo: self ].
	self endMethod
]

{ #category : 'API' }
ASOutput >> parentCodeGenerator: aCodeGenerator [
	parentCodeGenerator := aCodeGenerator
]

{ #category : 'API' }
ASOutput >> selector [

	^ codeGenerator selector
]
