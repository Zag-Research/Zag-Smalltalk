Class {
	#name : #PCBlock,
	#superclass : #PCObject,
	#instVars : [
		'outerContext',
		'startpc',
		'nargs'
	],
	#category : #'ASTKernel-Methods'
}

{ #category : #'as yet unclassified' }
PCBlock >> asContext [
	"Create a MethodContext that is ready to execute self.  Assumes self takes no args (if it does the args will be nil)"

	^ self asContextWithSender: nil
]

{ #category : #'as yet unclassified' }
PCBlock >> asContextWithSender: aContext [
	"Inner private support method for evaluation.  Do not use unless you know what you're doing."

	^ (PCMethodContext newForMethod: outerContext method)
		setSender: aContext
			receiver: outerContext receiver
			method: outerContext method
			closure: self
			startpc: startpc;
		privRefresh
]

{ #category : #'as yet unclassified' }
PCBlock >> home [
	^ outerContext home
]

{ #category : #'as yet unclassified' }
PCBlock >> ifError: errorHandlerBlock [
	"Evaluate the block represented by the receiver. If an error occurs the given handler block is evaluated. The handler block can be either a zero- or two-argument block; if the latter, then the error message and receiver are supplied to it as parameters. Answer the value returned by the handler block if the receiver gets an error."

	"Warning: The receiver should not contain an explicit return since that would leave an obsolete error handler hanging around."

	"Examples:
		[1 whatsUpDoc] ifError: [:err :rcvr | ^ 'huh?'].
		[1 whatsUpDoc] ifError: ['huh'].
		[1 / 0] ifError: [:err :rcvr |
			'division by 0' = err
				ifTrue: [^ Float infinity]
				ifFalse: [self error: err]]
"

	| activeProcess lastHandler val |
	activeProcess := Processor activeProcess.
	lastHandler := activeProcess errorHandler.
	activeProcess
		errorHandler: [ :aString :aReceiver | 
			activeProcess errorHandler: lastHandler.
			errorHandlerBlock numArgs = 0
				ifTrue: [ ^ errorHandlerBlock value ].
			^ errorHandlerBlock value: aString value: aReceiver ].
	val := self value.
	activeProcess errorHandler: lastHandler.
	^ val
]

{ #category : #'as yet unclassified' }
PCBlock >> method [
	^ self home method
]

{ #category : #'as yet unclassified' }
PCBlock >> numArgs [
	^ nargs
]

{ #category : #'as yet unclassified' }
PCBlock >> numCopiedValues [
	"Answer the number of copied values of the receiver.  Since these are
	 stored in the receiver's indexable fields this is the receiver's basic size.
	 Primitive. Answer the number of indexable variables in the receiver. 
	 This value is the same as the largest legal subscript."

	<primitive: 62>
	^ self basicSize
]

{ #category : #'as yet unclassified' }
PCBlock >> outerContext [
	^ outerContext
]

{ #category : #'as yet unclassified' }
PCBlock >> timeToRun [
	"Answer the number of milliseconds it took to evaluate this block."

	| startMSecs |
	startMSecs := PCSystem milliseconds.
	self value.
	^ PCSystem milliseconds - startMSecs
]

{ #category : #'as yet unclassified' }
PCBlock >> value [
	"Evaluate this block without any arguments."

	<primitive: 201>
	^ self valueWithArguments: #()
]

{ #category : #'as yet unclassified' }
PCBlock >> value: arg [
	"Evaluate this block with one argument."

	<primitive: 202>
	^ self valueWithArguments: (PCArray with: arg)
]

{ #category : #'as yet unclassified' }
PCBlock >> value: arg1 value: arg2 [
	"Evaluate this block with two arguments."

	<primitive: 203>
	^ self valueWithArguments: (PCArray with: arg1 with: arg2)
]

{ #category : #'as yet unclassified' }
PCBlock >> valueWithArguments: anArray [
	"Primitive. Evaluate the block represented by the receiver. The argument is an Array whose elements are the arguments for the block. Fail if the length of the Array is not the same as the the number of arguments that the block was expecting. Fail if the block is already being executed. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 206>
	anArray size = nargs
		ifTrue: [ self
				error: 'Attempt to evaluate a block that is already being evaluated.' ]
		ifFalse: [ self
				error: 'This block requires ' , nargs printString , ' arguments.' ]
]
