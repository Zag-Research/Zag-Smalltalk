Class {
	#name : #PCByteArray,
	#superclass : #PCArrayedCollection,
	#type : #bytes,
	#category : #'ASTKernel-Collections-Ordered'
}

{ #category : #'as yet unclassified' }
PCByteArray >> asByteArray [
	^ self
]

{ #category : #'as yet unclassified' }
PCByteArray >> asString [
	"Answer the receiver converted to a String."

	^ (PCString new: self size)
		replaceFrom: 1
		to: self size
		with: self
		startingAt: 1
]

{ #category : #'as yet unclassified' }
PCByteArray >> replaceFrom: startIndex to: stopIndex with: source startingAt: srcStartIndex [
	"Primitive. Destructively replace the elements from startIndex to stopIndex in the receiver with the elements starting at srcStartIndex in the source collection. Answer the receiver. Range checks are performed in the primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 105>
	super
		replaceFrom: startIndex
		to: stopIndex
		with: source
		startingAt: srcStartIndex
]
