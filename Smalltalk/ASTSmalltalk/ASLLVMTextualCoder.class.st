Class {
	#name : 'ASLLVMTextualCoder',
	#superclass : 'ASLLVMCoder',
	#instVars : [
		'encoder'
	],
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'as yet unclassified' }
ASLLVMTextualCoder >> assign: value [

	| variable |
	variable := self nextVariable.
	self
		nextPutAll: variable asString;
		nextPutAll: ' = ';
		nextPutAll: value.
	^ variable
]

{ #category : 'as yet unclassified' }
ASLLVMTextualCoder >> assignLiteral: value [

	| variable |
	variable := self nextVariable.
	self
		nextPutAll: variable asString;
		nextPutAll: ' = ';
		nextPutAll: (encoder encode: value).
	^ variable
]
