Class {
	#name : 'BeRiObject',
	#superclass : 'Object',
	#classVars : [
		'DeviceA',
		'DeviceB',
		'DevicePacketKind',
		'HandlerA',
		'HandlerB',
		'Idler',
		'WorkPacketKind',
		'Worker'
	],
	#category : 'Benchmarks-Richards',
	#package : 'Benchmarks',
	#tag : 'Richards'
}

{ #category : 'initialize' }
BeRiObject class >> initialize [
    "BeRiObject initialize"
    DeviceA := 5.
    DeviceB := 6.
    DevicePacketKind := 1.
    HandlerA := 3.
    HandlerB := 4.
    Idler := 1.
    Worker := 2.
    WorkPacketKind := 2
]

{ #category : 'utilities' }
BeRiObject >> append: packet head: queueHead [
    | mouse link |
    packet link: nil.
    queueHead isNil ifTrue: [^packet].
    mouse := queueHead.
    [(link := mouse link) isNil]
	whileFalse: [mouse := link].
    mouse link: packet.
    ^queueHead
]
