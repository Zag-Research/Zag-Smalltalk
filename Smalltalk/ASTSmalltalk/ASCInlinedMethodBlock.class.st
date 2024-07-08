"
I am an inline block, but I'm used to inline a BlockClosure
"
Class {
	#name : 'ASCInlinedMethodBlock',
	#superclass : 'ASCInlineBlock',
	#instVars : [
		'theMethod'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'adding' }
ASCInlinedMethodBlock >> copyStackFrom: aBasicBlock [

	^ stack := aBasicBlock stack.
	self nameStack: compiledMethod argumentNames
]

{ #category : 'initialization' }
ASCInlinedMethodBlock >> label [

	^ label ifNil: [
		  label := String streamContents: [ :s |
			           s
				           nextPutAll: super label;
				           nextPut: $_;
				           print: theMethod ] ]
]

{ #category : 'accessing' }
ASCInlinedMethodBlock >> theMethod: aCompiledMethod [

	theMethod := aCompiledMethod
]
