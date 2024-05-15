"
My subclasses output text versions of methods
"
Class {
	#name : 'ASZigMethodOutput',
	#superclass : 'ASZigOutput',
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'API' }
ASZigMethodOutput >> codeGenerator [

	^ codeGenerator ifNil: [
		  codeGenerator := self codeGeneratorClass new
			                   parentCodeGenerator: parentCodeGenerator;
			                   output: self;
			                   targetClass: forClass;
			                   enclosingScope: parentCodeGenerator ]
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

{ #category : 'output' }
ASZigMethodOutput >> label: result [

	delayedLabel = result ifFalse: [
		delayedLabel ifNotNil: [
			self
				putEmbedded: #br;
				labelRef: delayedLabel ] ].
	delayedLabel := nil.
	result ifNotNil: [
		self
			nextPutAll: '":';
			nextPutAll: result;
			nextPut: $";
			comma;
			lf ].
	^ result
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
	^ self putEmbedded: aSymbol; lf
]
