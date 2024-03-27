"
I am a reference to a parent Method/BlockClosure variable.

I wrap that in whatever is needed to access the parent's context.
"
Class {
	#name : 'ASCIndirectVariable',
	#superclass : 'ASCVariable',
	#instVars : [
		'variable'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'accessing' }
ASCIndirectVariable >> assertIsValue [
]

{ #category : 'accessing' }
ASCIndirectVariable >> makeManifest [
]

{ #category : 'accessing' }
ASCIndirectVariable >> offsets [
	^#(1 3 4)
]

{ #category : 'accessing' }
ASCIndirectVariable >> pop: output [

	^ output visitPopLocalData: self
]

{ #category : 'accessing' }
ASCIndirectVariable >> push: output [

	^ output visitPushLocalOrBlock: self
]

{ #category : 'accessing' }
ASCIndirectVariable >> push: aSymbol on: aStack [
	^ ASStackNamed new
		         from: aStack;
		         name: aSymbol 
]

{ #category : 'accessing' }
ASCIndirectVariable >> variable: aVariable [

	variable := aVariable
]
