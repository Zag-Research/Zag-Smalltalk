"
My subclasses output text versions of methods
"
Class {
	#name : 'ASZigMethodOutput',
	#superclass : 'ASZigOutput',
	#category : 'Zag-Output-Text-Output',
	#package : 'Zag-Output',
	#tag : 'Text-Output'
}

{ #category : 'API' }
ASZigMethodOutput >> codeGenerator [

		self deprecated: 'restructured'.
	^ codeGenerator ifNil: [
		  codeGenerator := self codeGeneratorClass new
			                   parentCodeGenerator: parentCodeGenerator;
			                   outputTo: self;
			                   inline: inline;
			                   targetClass: forClass;
			                   enclosingScope: parentCodeGenerator ]
]

{ #category : 'code generation' }
ASZigMethodOutput >> generateCode [

	alreadyGenerated ifTrue: [ ^ self ].
	alreadyGenerated := true.
	self codeGenerator generateFrom: ourObject.
	self outputToStream
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
ASZigMethodOutput >> outputMethodStart: bb [

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
ASZigMethodOutput >> visitReturnTop [
	^ self subclassResponsibility
]

{ #category : 'API' }
ASZigMethodOutput >> visitSend: aSelector returnIndex: index receiver: anObject [ 
	^ self subclassResponsibility
]

{ #category : 'visiting' }
ASZigMethodOutput >> visitSimple: aSymbol [ 
	^ self putEmbedded: aSymbol; lf
]
