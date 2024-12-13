"
I format an IRMethod to generate Zig code to create a method thread.
"
Class {
	#name : 'ASTThreadFormat',
	#superclass : 'Object',
	#instVars : [
		'stream',
		'method'
	],
	#category : 'Zag-Inspector-Inspect',
	#package : 'Zag-Inspector',
	#tag : 'Inspect'
}

{ #category : 'printing' }
ASTThreadFormat >> cr [
	^ stream cr
]

{ #category : 'printing/formatting' }
ASTThreadFormat >> ir: anIrMethod [

	| prevBranchLabel |
	stream := WriteStream with: ''.
	method := anIrMethod asAST.
	method primitive ifNotNil: [ :primitive |
		stream
			nextPutAll: '&p.p';
			nextPutAll: primitive asString;
			nextPutAll: ',Code.int(-1),';
			cr ].
	method needsContext ifTrue: [
		stream
			nextPutAll: '&p.pushContext,"^",';
			cr ].
	prevBranchLabel := nil.
	method allSequencesDo: [ :irSequence |
		prevBranchLabel == irSequence ifFalse: [
			prevBranchLabel threadFormatOn: stream.
			stream cr ].
		stream
			nextPutAll: '"label';
			nextPutAll: irSequence orderNumber asString;
			nextPutAll: ':",';
			cr.
		irSequence do: [ :irInstruction |
			prevBranchLabel := irInstruction threadFormatOn: self.
			stream cr ] ].
	prevBranchLabel threadFormatOn: stream.
	^ String streamContents: [ :s |
		  s
			  nextPutAll: 'const prog = compileTuple(sym.';
			  nextPutAll: method selector;
			  nextPut: $,;
			  nextPutAll: method numArgs asString;
			  nextPut: $,;
			  nextPutAll: method tempMap size asString;
			  nextPutAll: ',.{';
			  cr;
			  nextPutAll: stream contents;
			  cr;
			  nextPutAll: '};';
			  cr ]
]

{ #category : 'tests' }
ASTThreadFormat >> loop [

	| x |
	x := 1000000000.
	[ (x := x - 1) > 0 ] whileTrue: [ 
		x even
			ifTrue: [ x * 2 + 3 ]
			ifFalse: [ x * 3 + 1 ] ]
]

{ #category : 'printing' }
ASTThreadFormat >> nextPut: aCharacter [ 
	^ stream nextPut: aCharacter
]

{ #category : 'printing' }
ASTThreadFormat >> nextPutAll: aString [ 
	^ stream nextPutAll: aString
]

{ #category : 'printing' }
ASTThreadFormat >> printTempOffset: aSymbol [
	stream nextPutAll: (method tempMap at: aSymbol) asString
]

{ #category : 'accessing' }
ASTThreadFormat >> temporaryNames [
	| x |
	x := 42.
	^ x<0 ifTrue: [ 0 ] ifFalse: [ x ]
]
