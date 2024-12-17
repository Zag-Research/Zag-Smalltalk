Class {
	#name : 'BeRiTaskControlBlock',
	#superclass : 'BeRiTaskState',
	#instVars : [
		'link',
		'identity',
		'priority',
		'input',
		'state',
		'function',
		'handle'
	],
	#category : 'Benchmarks-Richards',
	#package : 'Benchmarks',
	#tag : 'Richards'
}

{ #category : 'instance creation' }
BeRiTaskControlBlock class >> link: link create: identity priority: priority initialWorkQueue: initialWorkQueue initialState: initialState function: aBlock privateData: privateData [ 
    ^super new
	link: link
	identity: identity
	priority: priority
	initialWorkQueue: initialWorkQueue
	initialState: initialState
	function: aBlock
	privateData: privateData
]

{ #category : 'scheduling' }
BeRiTaskControlBlock >> addInput: packet checkPriority: oldTask [
    input isNil
	ifTrue: 
	    [input := packet.
	    packetPendingIV := true.
	    priority > oldTask priority ifTrue: [^self]]
	ifFalse: 
	    [input := self append: packet head: input].
    ^oldTask
]

{ #category : 'accessing' }
BeRiTaskControlBlock >> identity [
    ^identity
]

{ #category : 'accessing' }
BeRiTaskControlBlock >> link [
    ^link
]

{ #category : 'initialize' }
BeRiTaskControlBlock >> link: aLink identity: anIdentity priority: aPriority initialWorkQueue: anInitialWorkQueue initialState: anInitialState function: aBlock privateData: aPrivateData [ 
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

{ #category : 'accessing' }
BeRiTaskControlBlock >> priority [
    ^priority
]

{ #category : 'scheduling' }
BeRiTaskControlBlock >> runTask [
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
