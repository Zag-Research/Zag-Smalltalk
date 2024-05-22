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
		'numLocals',
		'componentName',
		'blockNumber'
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

	^ blockNumber := blockNumber + 1
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
		generateCode: anASCBlock block for: BlockClosure
]

{ #category : 'API' }
ASOutput >> context: anASContext [

	context := anASContext.
	anASContext addComponent: self
]

{ #category : 'API' }
ASOutput >> endMethod [

]

{ #category : 'API' }
ASOutput >> forClass [

	^ forClass
]

{ #category : 'API' }
ASOutput >> generateCode [

	alreadyGenerated ifTrue: [ ^ self ].
	alreadyGenerated := true.
	ourObject ifNotNil: [
	self codeGenerator generate: ourObject]
]

{ #category : 'API' }
ASOutput >> generateCode: anASNode [

	context for: anASNode generatorIs: self.
	ourObject := anASNode.
	context processComponents
]

{ #category : 'API' }
ASOutput >> generateCode: anASNode for: aClass [

	context for: anASNode generatorIs: self.
	ourObject := anASNode.
	forClass := aClass.
	context processComponents
]

{ #category : 'initialization' }
ASOutput >> initialize [

	super initialize.
	alreadyGenerated := false.
	maxDepth := 0.
	blockNumber := 0
]

{ #category : 'as yet unclassified' }
ASOutput >> name [
	^ self subclassResponsibility
]

{ #category : 'API' }
ASOutput >> outputToStream [

	codeGenerator finalAnalysis.
	self setupForOutput.
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

{ #category : 'API' }
ASOutput >> setupForOutput [
]
