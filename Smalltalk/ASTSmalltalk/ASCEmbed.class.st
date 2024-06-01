"
I represent an embedded send replacement.
"
Class {
	#name : 'ASCEmbed',
	#superclass : 'ASCOperations',
	#instVars : [
		'selector'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'printing' }
ASCEmbed >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	aStream
		space;
		print: selector.
]

{ #category : 'instance creation' }
ASCEmbed >> next [
	^nil
]

{ #category : 'code generation' }
ASCEmbed >> outputTo: anOutput [

	^ anOutput visitEmbed: selector
]

{ #category : 'instance creation' }
ASCEmbed >> with: aSymbol [

	selector := aSymbol
]
