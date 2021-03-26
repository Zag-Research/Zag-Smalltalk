"
I write out ASTSmalltalk images
"
Class {
	#name : #ASImage,
	#superclass : #Object,
	#category : #'ASTSmalltalk-AST'
}

{ #category : #writing }
ASImage >> writeHeapOn: aStream [

]

{ #category : #writing }
ASImage >> writeImageOn: aStream [
	| heapSizePos symbolSizePos classSizePos |
	aStream nextPutAll: 'ASTSmall' asByteArray.
	heapSizePos := aStream position.
	aStream nextPutAll: 0 asRaw64Bits. " heap size "
	classSizePos := aStream position.
	aStream nextPutAll: 0 asRaw64Bits. " n classes "
	symbolSizePos := aStream position.
	aStream nextPutAll: 0 asRaw64Bits. " n symbols "
	self writeSizeOf: [self writeHeapOn: aStream ] at: heapSizePos on: aStream.
	self writeSizeOf: [self writeClassTable: aStream ] at: heapSizePos on: aStream.
	self writeSizeOf: [self writeSymbolTable: aStream ] at: heapSizePos on: aStream.
	
]

{ #category : #writing }
ASImage >> writeSizeOf: aBlock at: sizePosition on: aStream [
	| start pos |
	start := aStream position.
	aBlock value.
	pos := aStream position.
	aStream position: sizePosition.
	aStream nextPutAll: (pos-start) asRaw64Bits.
	aStream position: pos.

]
