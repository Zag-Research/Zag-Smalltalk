"

I am a sequence of statements which represents the body of a method or block.

Class {
	#name : 'ASSequence',
	#superclass : 'Array',
	#type : 'variable',
	#category : 'ASTSmalltalk-AST',
	#package : 'ASTSmalltalk',
	#tag : 'AST'
}

{ #category : 'comparing' }
ASSequence >> children [
	^ self
]

{ #category : 'filter streaming' }
ASSequence >> contents [
"create a stream to write the AST on and return it as a String"
	| stream |
	stream := WriteStream on: String new.
	self inspectZagASTOn: stream.

	^ stream contents
]

{ #category : 'comparing' }
ASSequence >> isImmediateThunk [

	^ self size == 1 and: [ self first isImmediateThunk ]
]

{ #category : 'printing' }
ASSequence >> printOn: aStream [
	^ self printAsSelfEvaluatingFormOn: aStream
]

{ #category : 'codegen' }
ASSequence >> zigWalkEmpty: emptyBlock rest: restBlock last: lastBlock [

|size|
size := self size.
self withIndexDo: [ : elem : idx | idx = size ifTrue: [ ^ lastBlock value: elem ] ifFalse: [ restBlock value: elem ] ].
 ^ emptyBlock value
]
