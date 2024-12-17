"
I represent the program counter pointing into a compiled method.
"
Class {
	#name : 'ASIProgramCounter',
	#superclass : 'ASINullProgramCounter',
	#instVars : [
		'currentIndex',
		'basicBlock'
	],
	#category : 'Zag-Core-Tests-SemanticInterpreter',
	#package : 'Zag-Core-Tests',
	#tag : 'SemanticInterpreter'
}

{ #category : 'interpreting' }
ASIProgramCounter >> doForInterpreter: anInterpreter [

	| operation |
	operation := basicBlock operationAt: currentIndex.
	currentIndex := currentIndex + 1.
	^ operation interpretFor: anInterpreter
]

{ #category : 'accessing' }
ASIProgramCounter >> forBasicBlock: aBasicBlock [

	basicBlock := aBasicBlock.
	currentIndex := 1
]

{ #category : 'printing' }
ASIProgramCounter >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPutAll: ' (';
		print: basicBlock;
		nextPutAll: ' #';
		print: currentIndex;
		nextPut: $)
]
