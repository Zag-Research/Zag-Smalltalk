Class {
	#name : #ASTBlockClosure,
	#superclass : #ASTObject,
	#instVars : [
		'symbol',
		'code',
		'closure',
		'selfObject'
	],
	#category : #'ASTKernel-Methods'
}

{ #category : #evaluating }
ASTBlockClosure >> cull: arg1 [
	"Evaluate this block with one or fewer arguments."

	^ self numArgs = 0
			ifTrue: [ self value ]
			ifFalse: [ self value: arg1]
]

{ #category : #evaluating }
ASTBlockClosure >> cull: arg1 cull: arg2 [
	"Evaluate this block with two or fewer arguments."

	^ self numArgs < 2
			ifTrue: [ self cull: arg1 ]
			ifFalse: [ self value: arg1 value: arg2]
]

{ #category : #evaluating }
ASTBlockClosure >> cull: arg1 cull: arg2 cull: arg3 [
	"Evaluate this block with three or fewer arguments."

	^ self numArgs < 3
			ifTrue: [ self cull: arg1 cull: arg2 ]
			ifFalse: [ self value: arg1 value: arg2 value: arg3]
]

{ #category : #evaluating }
ASTBlockClosure >> cull: arg1 cull: arg2 cull: arg3 cull: arg4 [
	"Evaluate this block with four or fewer arguments."

	^ self numArgs < 4
			ifTrue: [ self cull: arg1 cull: arg2 cull: arg3 ]
			ifFalse: [ self value: arg1 value: arg2 value: arg3 value: arg4 ]
]

{ #category : #exceptions }
ASTBlockClosure >> ifError: errorHandlerBlock [
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

{ #category : #accessing }
ASTBlockClosure >> numArgs [
	^ symbol numArgs
]

{ #category : #'error handling' }
ASTBlockClosure >> numArgsError: numArgsForInvocation [

	ArgumentsCountMismatch signalExpectedArgumentsCount: self numArgs calledArgumentsCount: numArgsForInvocation 
]

{ #category : #accessing }
ASTBlockClosure >> numCopiedValues [
	"Answer the number of copied values of the receiver.  Since these are
	 stored in the receiver's indexable fields this is the receiver's basic size.
	 Primitive. Answer the number of indexable variables in the receiver. 
	 This value is the same as the largest legal subscript."

	<primitive: 62>
	^ self basicSize
]

{ #category : #exceptions }
ASTBlockClosure >> on: exception do: handlerAction [
	"Evaluate the receiver in the scope of an exception handler.
	The following primitive is just a marker used to find the error handling context. 
	See MethodContext>>#isHandlerOrSignalingContext. "
	<primitive: 199>  
	^ self value
]

{ #category : #timing }
ASTBlockClosure >> timeToRun [
	"Answer the number of milliseconds it took to evaluate this block."

	| startMSecs |
	startMSecs := ASTSystem milliseconds.
	self value.
	^ ASTSystem milliseconds - startMSecs
]

{ #category : #evaluating }
ASTBlockClosure >> value [
	"Evaluate this block without any arguments."

	^ self numArgsError: 0
]

{ #category : #evaluating }
ASTBlockClosure >> value: arg [
	"Evaluate this block with one argument."

	^ self numArgsError: 1
]

{ #category : #evaluating }
ASTBlockClosure >> value: arg1 value: arg2 [
	"Evaluate this block with two arguments."

	^ self numArgsError: 2
]

{ #category : #evaluating }
ASTBlockClosure >> value: arg1 value: arg2 value: arg3 [
	"Evaluate this block with three arguments."

	^ self numArgsError: 3
]

{ #category : #evaluating }
ASTBlockClosure >> value: arg1 value: arg2 value: arg3 value: arg4 [
	"Evaluate this block with four arguments."

	^ self numArgsError: 4
]

{ #category : #evaluating }
ASTBlockClosure >> valueWithArguments: anArray [
	"Primitive. Evaluate the block represented by the receiver. The argument is an Array whose elements are the arguments for the block. Fail if the length of the Array is not the same as the the number of arguments that the block was expecting. Fail if the block is already being executed. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 206>
	anArray size = self numArgs
		ifTrue: [ self
				error: 'Attempt to evaluate a block that is already being evaluated.' ]
		ifFalse: [ self numArgsError: anArray size ]
]

{ #category : #controlling }
ASTBlockClosure >> whileFalse [

	"Evaluate the receiver, as long as its value is false."

	self value ifTrue: [^ self].
	^ self whileFalse
]

{ #category : #controlling }
ASTBlockClosure >> whileFalse: aBlock [

	"Evaluate the argument, aBlock, as long as the value of the receiver is false."

	self value ifTrue: [^ nil].
	aBlock value.
	^ self whileFalse: aBlock
]

{ #category : #controlling }
ASTBlockClosure >> whileNil: aBlock [

	"Evaluate the argument, aBlock, as long as the value of the receiver is nil."

	self value ifNotNil: [^ nil].
	aBlock value.
	^ self whileNil: aBlock
]

{ #category : #controlling }
ASTBlockClosure >> whileNotNil: aBlock [

	"Evaluate the argument, aBlock, as long as the value of the receiver is not nil."

	self value ifNil: [ ^ nil ].
	aBlock value.
	^ self whileNotNil: aBlock
]

{ #category : #controlling }
ASTBlockClosure >> whileTrue [

	"Evaluate the receiver, as long as its value is true."

	self value ifFalse: [^ self].
	^ self whileTrue
]

{ #category : #controlling }
ASTBlockClosure >> whileTrue: aBlock [

	"Evaluate the argument, aBlock, as long as the value of the receiver is true."

	self value ifFalse: [ ^ nil ].
	aBlock value.
	^ self whileTrue: aBlock
]
