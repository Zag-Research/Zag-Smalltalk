Class {
	#name : #PjRbTaskState,
	#superclass : #PjRbObject,
	#instVars : [
		'packetPendingIV',
		'taskWaiting',
		'taskHolding'
	],
	#category : #'Zag-Benchmarking-Richards'
}

{ #category : #'instance creation' }
PjRbTaskState class >> packetPending [
    ^super new packetPending
]

{ #category : #'instance creation' }
PjRbTaskState class >> running [
    ^super new running
]

{ #category : #'instance creation' }
PjRbTaskState class >> waiting [
    ^super new waiting
]

{ #category : #'instance creation' }
PjRbTaskState class >> waitingWithPacket [
    ^super new waitingWithPacket
]

{ #category : #accessing }
PjRbTaskState >> isPacketPending [
    ^packetPendingIV
]

{ #category : #testing }
PjRbTaskState >> isRunning [
    ^packetPendingIV not and: [taskWaiting not and: [taskHolding not]]
]

{ #category : #accessing }
PjRbTaskState >> isTaskHolding [
    ^taskHolding
]

{ #category : #testing }
PjRbTaskState >> isTaskHoldingOrWaiting [
    ^taskHolding or: [packetPendingIV not and: [taskWaiting]]
]

{ #category : #accessing }
PjRbTaskState >> isTaskWaiting [
    ^taskWaiting
]

{ #category : #testing }
PjRbTaskState >> isWaiting [
    ^packetPendingIV not and: [taskWaiting and: [taskHolding not]]
]

{ #category : #testing }
PjRbTaskState >> isWaitingWithPacket [
    ^packetPendingIV and: [taskWaiting and: [taskHolding not]]
]

{ #category : #initialization }
PjRbTaskState >> packetPending [
    packetPendingIV := true.
    taskWaiting := false.
    taskHolding := false
]

{ #category : #initialization }
PjRbTaskState >> running [
    packetPendingIV := taskWaiting := taskHolding := false
]

{ #category : #accessing }
PjRbTaskState >> taskHolding: aBoolean [
    taskHolding := aBoolean
]

{ #category : #accessing }
PjRbTaskState >> taskWaiting: aBoolean [ 
    taskWaiting := aBoolean
]

{ #category : #initialization }
PjRbTaskState >> waiting [
    packetPendingIV := taskHolding := false.
    taskWaiting := true
]

{ #category : #initialization }
PjRbTaskState >> waitingWithPacket [
    taskHolding := false.
    taskWaiting := packetPendingIV := true
]
