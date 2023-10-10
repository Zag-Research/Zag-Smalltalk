"
My subclasses output text versions of methods
"
Class {
	#name : #ASZigMethodOutput,
	#superclass : #ASZigOutput,
	#instVars : [
		'codeGenerator',
		'closureSize'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #'as yet unclassified' }
ASZigMethodOutput class >> testing [
	^ self subclassResponsibility
]

{ #category : #'code generation' }
ASZigMethodOutput >> codeGenerator [

	^ codeGenerator ifNil: [
		  codeGenerator := ASCodeGenerator new context: self.
		  breakAtCall ifTrue: [ codeGenerator breakAtCall ].
		  codeGenerator ]
]

{ #category : #API }
ASZigMethodOutput >> drop [
	^ self subclassResponsibility
]

{ #category : #'code generation' }
ASZigMethodOutput >> generateCode [

	alreadyGenerated ifTrue: [ ^ self ].
	alreadyGenerated := true.
	ourObject zigCompileMethod: self codeGenerator.
	self outputToStream
]

{ #category : #initialization }
ASZigMethodOutput >> initialize [

	super initialize.
	closureSize := 0
]

{ #category : #'code generation' }
ASZigMethodOutput >> outputToStream [ 

self subclassResponsibility 
]

{ #category : #API }
ASZigMethodOutput >> popLocal: localNumber [ 
	^ self subclassResponsibility
]

{ #category : #API }
ASZigMethodOutput >> popLocalData: name stackIndex: stackIndex blockIndex: blockIndex [ 
	^ self subclassResponsibility
]

{ #category : #accessing }
ASZigMethodOutput >> prefix [ ^ 'M'
]

{ #category : #API }
ASZigMethodOutput >> pushBlock: stackIndex [ 
	^ self subclassResponsibility
]

{ #category : #API }
ASZigMethodOutput >> pushContext [
	^ self subclassResponsibility
]

{ #category : #API }
ASZigMethodOutput >> pushLiteral: aLiteral [ 
	^ self subclassResponsibility
]

{ #category : #API }
ASZigMethodOutput >> pushLocal: localNumber [ 
	^ self subclassResponsibility
]

{ #category : #API }
ASZigMethodOutput >> returnTop [
	^ self subclassResponsibility
]

{ #category : #accessing }
ASZigMethodOutput >> selector [

	^ codeGenerator selector
]

{ #category : #API }
ASZigMethodOutput >> send: aSelector useCall: useCall [ 
	^ self subclassResponsibility
]

{ #category : #API }
ASZigMethodOutput >> startMethod: primitiveNumber [ 
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
