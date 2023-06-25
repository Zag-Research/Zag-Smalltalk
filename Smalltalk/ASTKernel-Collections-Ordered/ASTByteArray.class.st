Class {
	#name : #ASTByteArray,
	#superclass : #ASTArrayedCollection,
	#type : #bytes,
	#category : #'ASTKernel-Collections-Ordered'
}

{ #category : #'as yet unclassified' }
ASTByteArray >> asByteArray [
	^ self
]

{ #category : #converting }
ASTByteArray >> asString [
	"Answer the receiver converted to a String."

	^ (ASTString new: self size)
		replaceFrom: 1
		to: self size
		with: self
		startingAt: 1
]

{ #category : #'system primitives' }
ASTByteArray >> replaceFrom: startIndex to: stopIndex with: source startingAt: srcStartIndex [
	"Primitive. Destructively replace the elements from startIndex to stopIndex in the receiver with the elements starting at srcStartIndex in the source collection. Answer the receiver. Range checks are performed in the primitive. Optional. See Object documentation whatIsAPrimitive."

	<primitive: 105>
	super
		replaceFrom: startIndex
		to: stopIndex
		with: source
		startingAt: srcStartIndex
]
