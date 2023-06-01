Class {
	#name : #PjRbPacket,
	#superclass : #PjRbObject,
	#instVars : [
		'link',
		'identity',
		'kind',
		'datum',
		'data'
	],
	#category : #'Zag-Benchmarking-Richards'
}

{ #category : #'instance creation' }
PjRbPacket class >> create: link identity: identity kind: kind [ 
    ^super new
	link: link
	identity: identity
	kind: kind
]

{ #category : #accessing }
PjRbPacket >> data [
    ^data
]

{ #category : #accessing }
PjRbPacket >> datum [
    ^datum
]

{ #category : #accessing }
PjRbPacket >> datum:    someData [
    datum := someData
]

{ #category : #accessing }
PjRbPacket >> identity [
    ^identity
]

{ #category : #accessing }
PjRbPacket >> identity: anIdentity [
    identity := anIdentity
]

{ #category : #accessing }
PjRbPacket >> kind [
    ^kind
]

{ #category : #accessing }
PjRbPacket >> link [
    ^link
]

{ #category : #accessing }
PjRbPacket >> link: aWorkQueue [ 
    link := aWorkQueue
]

{ #category : #initialization }
PjRbPacket >> link: aLink identity: anIdentity kind: aKind [ 
    link := aLink.
    identity := anIdentity. 
    kind := aKind.
    datum := 1.
    data := ByteArray new: 4
]
