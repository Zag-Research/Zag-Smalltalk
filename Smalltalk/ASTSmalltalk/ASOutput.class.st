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
		'blockNumber',
		'converter',
		'inline'
	],
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'as yet unclassified' }
ASOutput class >> withInlining [
	^ self new inline: true
]

{ #category : 'API' }
ASOutput >> blockClass [
	^ self class blockClass
]

{ #category : 'as yet unclassified' }
ASOutput >> blockGenerator [

	^ self blockClass new
		  testing: testing;
		  context: context;
		  methodGenerator: self methodGenerator block: self blockNumber
]

{ #category : 'API' }
ASOutput >> blockNumber [

	^ blockNumber := blockNumber + 1
]

{ #category : 'API' }
ASOutput >> codeGenerator [

"	^ self shouldNotImplement 
"	^ codeGenerator ifNil: [
		  codeGenerator := self codeGeneratorClass new
			                   parentCodeGenerator: parentCodeGenerator;
			                   targetClass: forClass;
			                   inline: inline ]
]

{ #category : 'accessing' }
ASOutput >> codeGenerator: anASCodeGenerator [ 

	codeGenerator := anASCodeGenerator.
	^ anASCodeGenerator
]

{ #category : 'API' }
ASOutput >> codeGeneratorClass [ ^ ASCodeGenerator
]

{ #category : 'API' }
ASOutput >> context: anASContext [

	context := anASContext.
	anASContext addComponent: self.
	converter := nil "anASContext converter"
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
	self codeGenerator generateFrom: ourObject]
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
	blockNumber := 0.
	inline := false
]

{ #category : 'API' }
ASOutput >> inline: aBoolean [
	inline := aBoolean
]

{ #category : 'as yet unclassified' }
ASOutput >> methodGenerator [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
ASOutput >> name [
	^ self subclassResponsibility
]

{ #category : 'API' }
ASOutput >> outputMethodEnd [

]

{ #category : 'API' }
ASOutput >> outputToStream [

	codeGenerator finalAnalysis.
	self setupForOutput.
	codeGenerator basicBlocksLocalizeDo: [ :bb |
		currentBasicBlock := bb.
		maxDepth := maxDepth max: currentBasicBlock maxDepth.
		currentBasicBlock outputTo: self ].
	self outputMethodEnd
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
