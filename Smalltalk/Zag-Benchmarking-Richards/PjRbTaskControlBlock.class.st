Class {
	#name : #PjRbTaskControlBlock,
	#superclass : #PjRbTaskState,
	#instVars : [
		'link',
		'identity',
		'priority',
		'input',
		'state',
		'function',
		'handle'
	],
	#category : #'Zag-Benchmarking-Richards'
}

{ #category : #'instance creation' }
PjRbTaskControlBlock class >> link: link create: identity priority: priority initialWorkQueue: initialWorkQueue initialState: initialState function: aBlock privateData: privateData [ 
    ^super new
	link: link
	identity: identity
	priority: priority
	initialWorkQueue: initialWorkQueue
	initialState: initialState
	function: aBlock
	privateData: privateData
]

{ #category : #scheduling }
PjRbTaskControlBlock >> addInput: packet checkPriority: oldTask [
    input isNil
	ifTrue: 
	    [input := packet.
	    packetPendingIV := true.
	    priority > oldTask priority ifTrue: [^self]]
	ifFalse: 
	    [input := self append: packet head: input].
    ^oldTask
]

{ #category : #accessing }
PjRbTaskControlBlock >> identity [
    ^identity
]

{ #category : #accessing }
PjRbTaskControlBlock >> link [
    ^link
]

{ #category : #initialization }
PjRbTaskControlBlock >> link: aLink identity: anIdentity priority: aPriority initialWorkQueue: anInitialWorkQueue initialState: anInitialState function: aBlock privateData: aPrivateData [ 
    link := aLink.
    identity := anIdentity.
    priority := aPriority.
    input := anInitialWorkQueue.
    packetPendingIV := anInitialState isPacketPending.
    taskWaiting := anInitialState isTaskWaiting.
    taskHolding := anInitialState isTaskHolding.
    function := aBlock.
    handle := aPrivateData
]

{ #category : #accessing }
PjRbTaskControlBlock >> priority [
    ^priority
]

{ #category : #scheduling }
PjRbTaskControlBlock >> runTask [
    | message |
    self isWaitingWithPacket
	ifTrue: 
	    [message := input.
	    input := message link.
	    input isNil
		ifTrue: [self running]
		ifFalse: [self packetPending]]
	ifFalse: [message := nil].
    ^function value: message value: handle
]
