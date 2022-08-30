"
I format an IRMethod to generate Zig code to create a method thread.
"
Class {
	#name : #ASTThreadFormat,
	#superclass : #Object,
	#instVars : [
		'stream',
		'method'
	],
	#category : #'ASTSmalltalk-Threaded'
}

{ #category : #printing }
ASTThreadFormat >> cr [
	^ stream cr
]

{ #category : #'printing/formatting' }
ASTThreadFormat >> ir: anIRMethod [

	| prevBranchLabel |
	stream := WriteStream with: ''.
	method := anIRMethod.
	anIRMethod compiledMethod primitive ~= 0 ifTrue: [ 
		stream
			nextPutAll: 'p.p';
			nextPutAll: anIRMethod compiledMethod primitive asString;
			nextPutAll: ',Code.int(-1),';
			cr ].
	stream
		nextPutAll: 'p.pushContext,"^",';
		cr.
	prevBranchLabel := nil.
	anIRMethod startSequence withAllSuccessorsDo: [ :irSequence | 
		prevBranchLabel == irSequence ifFalse: [ 
			prevBranchLabel threadFormatOn: stream.
			stream cr ].
		stream
			nextPutAll: '"label';
			nextPutAll: irSequence orderNumber asString;
			nextPutAll: ':",';
			cr.
		irSequence sequence do: [ :irInstruction | 
			prevBranchLabel := irInstruction threadFormatOn: self.
			stream cr ] ].
	prevBranchLabel threadFormatOn: stream.
	^ String streamContents: [ :s | 
		  s
			  nextPutAll: 'const prog = compileTuple(sym.';
			  nextPutAll: anIRMethod sourceNode selector;
			  nextPut: $,;
			  nextPutAll: anIRMethod numArgs asString;
			  nextPut: $,;
			  nextPutAll: anIRMethod tempMap size asString;
			  nextPutAll: ',.{';
			  cr;
			  nextPutAll: stream contents;
			cr;	
			  nextPutAll: '};';
			  cr ]
]

{ #category : #tests }
ASTThreadFormat >> loop [

	| x |
	x := 100000000.
	[ (x := x - 1) > 0 ] whileTrue: [ 
		x even
			ifTrue: [ x * 2 + 3 ]
			ifFalse: [ x * 3 + 1 ] ]
]

{ #category : #printing }
ASTThreadFormat >> nextPut: aCharacter [ 
	^ stream nextPut: aCharacter
]

{ #category : #printing }
ASTThreadFormat >> nextPutAll: aString [ 
	^ stream nextPutAll: aString
]

{ #category : #printing }
ASTThreadFormat >> printTempOffset: aSymbol [
	stream nextPutAll: (method tempMap at: aSymbol) asString
]

{ #category : #accessing }
ASTThreadFormat >> temporaryNames [
	| x |
	x := 42.
	^ x<0 ifTrue: [ 0 ] ifFalse: [ x ]
]
