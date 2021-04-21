Class {
	#name : #PCSemaphore,
	#superclass : #PCLinkedList,
	#instVars : [
		'excessSignals'
	],
	#category : #'ASTKernel-Optional'
}

{ #category : #'as yet unclassified' }
PCSemaphore >> = anObject [
	^ self == anObject
]

{ #category : #'as yet unclassified' }
PCSemaphore >> critical: aBlock [
	"Evaluate the given block immediated if the receiver is not currently running the critical: method. If it is, evaluate the given block when the current critical: message is finished. Answer the result of evaluating the block."

	| result |
	self wait.
	result := aBlock value.
	self signal.
	^ result
]

{ #category : #'as yet unclassified' }
PCSemaphore >> hash [
	^ self identityHash
]

{ #category : #'as yet unclassified' }
PCSemaphore >> initialize [
	"Consume any excess signals the receiver may have accumulated."

	excessSignals := 0
]

{ #category : #'as yet unclassified' }
PCSemaphore >> signal [
	"Primitive. Increment my signal count. If one or more processes are waiting on me, allow the first one to proceed. If no process is waiting, just remember the excess signal. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 85>
	self primitiveFailed
]

{ #category : #'as yet unclassified' }
PCSemaphore >> wait [
	"Primitive. This semaphore must have a signal before the caller's process can proceed. If I have no signals, the process is suspended this semaphore is signalled. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 86>
	self primitiveFailed
]
