Class {
	#name : 'ASLLVMCoder',
	#superclass : 'Object',
	#instVars : [
		'sequence'
	],
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'as yet unclassified' }
ASLLVMCoder >> assign: value [
	^ self subclassResponsibility 
]

{ #category : 'as yet unclassified' }
ASLLVMCoder >> iniitialize [
	super initialize.
	sequence := 0
]

{ #category : 'as yet unclassified' }
ASLLVMCoder >> nextVariable [
	sequence := sequence + 1.
	^ '%v',sequence asString
]
