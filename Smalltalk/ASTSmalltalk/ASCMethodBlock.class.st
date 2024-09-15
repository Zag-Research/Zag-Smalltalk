"
I am an inline block, but I'm used to inline a BlockClosure
"
Class {
	#name : 'ASCMethodBlock',
	#superclass : 'ASCCompilableUnitBlock',
	#instVars : [
		'theMethod'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'adding' }
ASCMethodBlock >> copyStackFrom: aBasicBlock [

	stack := aBasicBlock stack.
	self nameStack: theMethod argumentNames.
	^ stack
]

{ #category : 'as yet unclassified' }
ASCMethodBlock >> label [

	^ label ifNil: [
		  label := String streamContents: [ :s |
			           s
				           nextPutAll: super label;
				           nextPut: $_;
				           print: theMethod ] ]
]

{ #category : 'as yet unclassified' }
ASCMethodBlock >> theMethod: aCompiledMethod [

	theMethod := aCompiledMethod
]
