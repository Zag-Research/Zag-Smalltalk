"
I contain a sequence of statements that make up, among other things, the body of a method
"
Class {
	#name : 'ASSequence',
	#superclass : 'Array',
	#type : 'variable',
	#category : 'ASTSmalltalk-AST',
	#package : 'ASTSmalltalk',
	#tag : 'AST'
}

{ #category : 'converting' }
ASSequence >> asASSequence [

	^ self
]

{ #category : 'testing' }
ASSequence >> isImmediateThunk [

	^ self size == 1 and: [ self first isImmediateThunk ]
]

{ #category : 'printing' }
ASSequence >> printOn: aStream [
	^ self printAsSelfEvaluatingFormOn: aStream
]

{ #category : 'codegen' }
ASSequence >> zigWalkEmpty: emptyBlock rest: restBlock last: lastBlock [

	| size |
	size := self size.
	self withIndexDo: [ :elem :idx |
		idx = size
			ifTrue: [ ^ lastBlock value: elem ]
			ifFalse: [ restBlock value: elem ] ].
	^ emptyBlock value
]
