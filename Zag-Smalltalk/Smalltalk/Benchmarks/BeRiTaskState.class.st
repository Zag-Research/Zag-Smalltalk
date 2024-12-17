Class {
	#name : 'BeRiTaskState',
	#superclass : 'BeRiObject',
	#instVars : [
		'packetPendingIV',
		'taskWaiting',
		'taskHolding'
	],
	#category : 'Benchmarks-Richards',
	#package : 'Benchmarks',
	#tag : 'Richards'
}

{ #category : 'instance creation' }
BeRiTaskState class >> packetPending [
    ^super new packetPending
]

{ #category : 'instance creation' }
BeRiTaskState class >> running [
    ^super new running
]

{ #category : 'instance creation' }
BeRiTaskState class >> waiting [
    ^super new waiting
]

{ #category : 'instance creation' }
BeRiTaskState class >> waitingWithPacket [
    ^super new waitingWithPacket
]

{ #category : 'accessing' }
BeRiTaskState >> isPacketPending [
    ^packetPendingIV
]

{ #category : 'testing' }
BeRiTaskState >> isRunning [
    ^packetPendingIV not and: [taskWaiting not and: [taskHolding not]]
]

{ #category : 'accessing' }
BeRiTaskState >> isTaskHolding [
    ^taskHolding
]

{ #category : 'testing' }
BeRiTaskState >> isTaskHoldingOrWaiting [
    ^taskHolding or: [packetPendingIV not and: [taskWaiting]]
]

{ #category : 'accessing' }
BeRiTaskState >> isTaskWaiting [
    ^taskWaiting
]

{ #category : 'testing' }
BeRiTaskState >> isWaiting [
    ^packetPendingIV not and: [taskWaiting and: [taskHolding not]]
]

{ #category : 'testing' }
BeRiTaskState >> isWaitingWithPacket [
    ^packetPendingIV and: [taskWaiting and: [taskHolding not]]
]

{ #category : 'initialize' }
BeRiTaskState >> packetPending [
    packetPendingIV := true.
    taskWaiting := false.
    taskHolding := false
]

{ #category : 'initialize' }
BeRiTaskState >> running [
    packetPendingIV := taskWaiting := taskHolding := false
]

{ #category : 'accessing' }
BeRiTaskState >> taskHolding: aBoolean [
    taskHolding := aBoolean
]

{ #category : 'accessing' }
BeRiTaskState >> taskWaiting: aBoolean [ 
    taskWaiting := aBoolean
]

{ #category : 'initialize' }
BeRiTaskState >> waiting [
    packetPendingIV := taskHolding := false.
    taskWaiting := true
]

{ #category : 'initialize' }
BeRiTaskState >> waitingWithPacket [
    taskHolding := false.
    taskWaiting := packetPendingIV := true
]
