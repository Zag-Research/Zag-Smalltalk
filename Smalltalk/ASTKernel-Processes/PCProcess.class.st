Class {
	#name : #PCProcess,
	#superclass : #PCObject,
	#instVars : [
		'nextLink',
		'suspendedContext',
		'priority',
		'myList',
		'threadId',
		'errorHandler'
	],
	#category : #'ASTKernel-Processes'
}

{ #category : #'as yet unclassified' }
PCProcess class >> for: aContext priority: anInteger [
	"Answer an instance of me for the given context (usually a Block) at the given priority."

	^ self new
		initSuspendedContext: aContext;
		priority: anInteger
]

{ #category : #'as yet unclassified' }
PCProcess >> errorHandler [
	^ errorHandler
]

{ #category : #'as yet unclassified' }
PCProcess >> errorHandler: aBlock [
	errorHandler := aBlock
]

{ #category : #'as yet unclassified' }
PCProcess >> initSuspendedContext: aContext [
	suspendedContext := aContext.
	priority := 1	"lowest priority"
]

{ #category : #'as yet unclassified' }
PCProcess >> nextLink [
	^ nextLink
]

{ #category : #'as yet unclassified' }
PCProcess >> nextLink: aLink [
	nextLink := aLink
]

{ #category : #'as yet unclassified' }
PCProcess >> printOn: aStream [
	super printOn: aStream.
	aStream nextPutAll: ' in '.
	suspendedContext printOn: aStream
]

{ #category : #'as yet unclassified' }
PCProcess >> priority [
	"Answer the priority of the receiver."

	^ priority
]

{ #category : #'as yet unclassified' }
PCProcess >> priority: anInteger [
	"Set the receiver's priority to anInteger. The priority is used by the VM as an index into the scheduler's array of process queues so it must be an integer between 1 and Processor highestPriority."

	priority := (anInteger asInteger max: 1)
		min: Processor highestPriority
]

{ #category : #'as yet unclassified' }
PCProcess >> resume [
	"Primitive. Allow this process to proceed. Put the receiver in line to become the active process. Fail if the receiver is already waiting on a queue (i.e., on a Semaphore or on a scheduler queue). Essential. See Object documentation whatIsAPrimitive."

	<primitive: 87>
	self primitiveFailed
]

{ #category : #'as yet unclassified' }
PCProcess >> suspend [
	"Primitive. Stop this process in such a way that it can be restarted later (see resume). If the receiver is the active process, suspend it. Otherwise, remove the receiver from its suspended process list. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 88>
	Processor activeProcess == self
		ifTrue: [ self primitiveFailed ]
		ifFalse: [ Processor
				remove: self
				ifAbsent: [ self error: 'This process was not active' ].
			myList := nil ]
]

{ #category : #'as yet unclassified' }
PCProcess >> suspendedContext [
	^ suspendedContext
]

{ #category : #'as yet unclassified' }
PCProcess >> terminate [
	"Stop this process forever."

	Processor activeProcess == self
		ifTrue: [ thisContext removeSelf suspend ]
		ifFalse: [ myList
				ifNotNil: [ myList remove: self ifAbsent: [  ].
					myList := nil ].
			suspendedContext := nil ]
]
