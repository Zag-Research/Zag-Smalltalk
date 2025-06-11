Class {
	#name : 'BeRiPacket',
	#superclass : 'BeRiObject',
	#instVars : [
		'link',
		'identity',
		'kind',
		'datum',
		'data'
	],
	#category : 'Benchmarks-Richards',
	#package : 'Benchmarks',
	#tag : 'Richards'
}

{ #category : 'instance creation' }
BeRiPacket class >> create: link identity: identity kind: kind [ 
    ^super new
	link: link
	identity: identity
	kind: kind
]

{ #category : 'accessing' }
BeRiPacket >> data [
    ^data
]

{ #category : 'accessing' }
BeRiPacket >> datum [
    ^datum
]

{ #category : 'accessing' }
BeRiPacket >> datum:    someData [
    datum := someData
]

{ #category : 'accessing' }
BeRiPacket >> identity [
    ^identity
]

{ #category : 'accessing' }
BeRiPacket >> identity: anIdentity [
    identity := anIdentity
]

{ #category : 'accessing' }
BeRiPacket >> kind [
    ^kind
]

{ #category : 'accessing' }
BeRiPacket >> link [
    ^link
]

{ #category : 'accessing' }
BeRiPacket >> link: aWorkQueue [ 
    link := aWorkQueue
]

{ #category : 'initialize' }
BeRiPacket >> link: aLink identity: anIdentity kind: aKind [ 
    link := aLink.
    identity := anIdentity. 
    kind := aKind.
    datum := 1.
    data := ByteArray new: 4
]
