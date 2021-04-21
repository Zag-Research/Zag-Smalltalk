Class {
	#name : #ASTSemaphore,
	#superclass : #ASTLinkedList,
	#instVars : [
		'excessSignals'
	],
	#category : #'ASTKernel-Optional'
}

{ #category : #'as yet unclassified' }
ASTSemaphore >> = anObject [
	^ self == anObject
]

{ #category : #'as yet unclassified' }
ASTSemaphore >> critical: aBlock [
	"Evaluate the given block immediated if the receiver is not currently running the critical: method. If it is, evaluate the given block when the current critical: message is finished. Answer the result of evaluating the block."

	| result |
	self wait.
	result := aBlock value.
	self signal.
	^ result
]

{ #category : #'as yet unclassified' }
ASTSemaphore >> hash [
	^ self identityHash
]

{ #category : #'as yet unclassified' }
ASTSemaphore >> initialize [
	"Consume any excess signals the receiver may have accumulated."

	excessSignals := 0
]

{ #category : #'as yet unclassified' }
ASTSemaphore >> signal [
	"Primitive. Increment my signal count. If one or more processes are waiting on me, allow the first one to proceed. If no process is waiting, just remember the excess signal. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 85>
	self primitiveFailed
]

{ #category : #'as yet unclassified' }
ASTSemaphore >> wait [
	"Primitive. This semaphore must have a signal before the caller's process can proceed. If I have no signals, the process is suspended this semaphore is signalled. Essential. See Object documentation whatIsAPrimitive."

	<primitive: 86>
	self primitiveFailed
]
