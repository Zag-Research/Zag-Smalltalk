"
I am an inline block, but I'm used to inline a BlockClosure
"
Class {
	#name : 'ASCInlinedMethodBlock',
	#superclass : 'ASCInlinedJoinBlock',
	#instVars : [
		'theMethod'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'adding' }
ASCInlinedMethodBlock >> copyStackFrom: aBasicBlock [

	stack := aBasicBlock stack.
	self nameStack: theMethod argumentNames.
	^ stack
]

{ #category : 'as yet unclassified' }
ASCInlinedMethodBlock >> label [

	^ label ifNil: [
		  label := String streamContents: [ :s |
			           s
				           nextPutAll: super label;
				           nextPut: $_;
				           print: theMethod ] ]
]

{ #category : 'as yet unclassified' }
ASCInlinedMethodBlock >> theMethod: aCompiledMethod [

	theMethod := aCompiledMethod
]
