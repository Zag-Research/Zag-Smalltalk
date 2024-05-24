"
I implement very simple operations described by a symbol
"
Class {
	#name : 'ASCSimple',
	#superclass : 'ASCOperations',
	#instVars : [
		'symbol'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'printing' }
ASCSimple >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	aStream << $(.
	aStream print: symbol.
	aStream << $)
]

{ #category : 'accessing' }
ASCSimple >> next [
	^ nil
]

{ #category : 'code generation' }
ASCSimple >> outputTo: aCodeEmitter [

	^ aCodeEmitter visitSimple: symbol
]

{ #category : 'instance creation' }
ASCSimple >> with: anObject [

	 symbol := anObject
]
