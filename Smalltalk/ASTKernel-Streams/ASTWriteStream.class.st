Class {
	#name : 'ASTWriteStream',
	#superclass : 'ASTReadStream',
	#instVars : [
		'writeLimit'
	],
	#category : 'ASTKernel-Streams',
	#package : 'ASTKernel-Streams'
}

{ #category : 'as yet unclassified' }
ASTWriteStream >> contents [
	"Answer with a copy of my collection up to the high-water mark that was written."

	readLimit := readLimit max: position.
	^ collection copyFrom: 1 to: readLimit
]

{ #category : 'as yet unclassified' }
ASTWriteStream >> nextPut: anObject [
	"Primitive. Insert the argument at the next position in the Stream represented by the receiver. Fail if the collection of this stream is not an Array or a String. Fail if the stream is positioned at its end, or if the position is out of bounds in the collection. Fail if the argument is not of the right type for the collection. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 66>
	position >= writeLimit
		ifTrue: [ ^ self pastEndPut: anObject ]
		ifFalse: [ position := position + 1.
			^ collection at: position put: anObject ]
]

{ #category : 'as yet unclassified' }
ASTWriteStream >> nextPutAll: aCollection [
	"Write the elements of the given collection starting at my current position. Answer the collection."

	"Optimization: If the given collection has the same class as my collection, use the fast operation replaceFrom:to:with:."

	| newEnd |
	collection class == aCollection class
		ifFalse: [ aCollection do: [ :v | self nextPut: v ].
			^ aCollection ].
	newEnd := position + aCollection size.
	newEnd > writeLimit
		ifTrue: [ collection := collection
				,
					(collection species
						new: newEnd - writeLimit + (collection size max: 20)).
			writeLimit := collection size ].
	"grow my collection if necessary"
	collection replaceFrom: position + 1 to: newEnd with: aCollection.
	position := newEnd
]

{ #category : 'as yet unclassified' }
ASTWriteStream >> on: aCollection [
	super on: aCollection.
	readLimit := 0.
	writeLimit := aCollection size
]

{ #category : 'as yet unclassified' }
ASTWriteStream >> pastEndPut: anObject [
	"Grow my collection."

	"Details: In general, double my size. Grow by at least 20 elements if my size is under 20 and grow by 20000 if my size is over 20000."

	collection := collection
		, (collection class new: ((collection size max: 20) min: 20000)).
	writeLimit := collection size.
	collection at: (position := position + 1) put: anObject
]

{ #category : 'as yet unclassified' }
ASTWriteStream >> position: anInteger [
	"Set my read position, but remember the high-water mark that was written."

	readLimit := readLimit max: position.
	super position: anInteger
]

{ #category : 'accessing' }
ASTWriteStream >> size [
	^ readLimit := readLimit max: position
]

{ #category : 'as yet unclassified' }
ASTWriteStream >> space [
	"Append a space character to me."

	self nextPut: ASTCharacter space
]
