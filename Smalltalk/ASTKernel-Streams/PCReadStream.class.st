Class {
	#name : #PCReadStream,
	#superclass : #PCObject,
	#instVars : [
		'collection',
		'position',
		'readLimit'
	],
	#category : #'ASTKernel-Streams'
}

{ #category : #'as yet unclassified' }
PCReadStream class >> on: aCollection [
	"Answer an instance of me, streaming over the elements of aCollection."

	^ self basicNew on: aCollection
]

{ #category : #'as yet unclassified' }
PCReadStream >> atEnd [
	"Primitive. Answer whether the receiver can access any more objects. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 67>
	^ position >= readLimit
]

{ #category : #'as yet unclassified' }
PCReadStream >> contents [
	"Answer with a copy of my collection from 1 to readLimit."

	^ collection copyFrom: 1 to: readLimit
]

{ #category : #'as yet unclassified' }
PCReadStream >> next [
	"Primitive. Answer the next object in the Stream represented by the receiver. Fail if the collection of this stream is not an Array or a String. Fail if the stream is positioned at its end, or if the position is out of bounds in the collection. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 65>
	position >= readLimit
		ifTrue: [ ^ nil ]
		ifFalse: [ ^ collection at: (position := position + 1) ]
]

{ #category : #'as yet unclassified' }
PCReadStream >> next: anInteger [
	"Answer a collection containing the next anInteger elements of my collection."

	| end result |
	end := position + anInteger min: readLimit.
	result := collection copyFrom: position + 1 to: end.
	position := end.
	^ result
]

{ #category : #'as yet unclassified' }
PCReadStream >> on: aCollection [
	"Initialize myself for streaming over the given collection."

	collection := aCollection.
	readLimit := aCollection size.
	position := 0
]

{ #category : #'as yet unclassified' }
PCReadStream >> peek [
	"Answer the next object without advancing my position. Answer nil if there are no more elements."

	| result |
	self atEnd
		ifTrue: [ ^ nil ].
	result := self next.
	position := position - 1.
	^ result
]

{ #category : #'as yet unclassified' }
PCReadStream >> peekFor: anObject [
	"If my next element equals the given object, skip it and answer true. Otherwise, answer false and leave my position unchanged."

	| result |
	result := self peek = anObject.
	result
		ifTrue: [ self skip: 1 ].
	^ result
]

{ #category : #'as yet unclassified' }
PCReadStream >> position [
	"Answer the current position of accessing the sequence of objects."

	^ position
]

{ #category : #'as yet unclassified' }
PCReadStream >> position: anInteger [
	"Set my current position to anInteger, as long as anInteger is within bounds. If not, report an error."

	anInteger >= 0 & (anInteger <= readLimit)
		ifTrue: [ position := anInteger asInteger ]
		ifFalse: [ self error: 'Position out of bounds: ' , anInteger printString ]
]

{ #category : #'as yet unclassified' }
PCReadStream >> size [
	"Compatibility with other streams (e.g., FileStream)"

	^ readLimit
]

{ #category : #'as yet unclassified' }
PCReadStream >> skip: anInteger [
	"Set the receiver's position to be the current position+anInteger."

	self position: (position + anInteger min: readLimit)
]
