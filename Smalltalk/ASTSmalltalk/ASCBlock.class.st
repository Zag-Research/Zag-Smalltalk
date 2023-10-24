"
I hold a ASBlock AST, to prepare for analysis
"
Class {
	#name : #ASCBlock,
	#superclass : #ASCOperations,
	#instVars : [
		'block',
		'manifest'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #accessing }
ASCBlock >> block [
	^ block
]

{ #category : #initialization }
ASCBlock >> initialize [ 
super initialize .
manifest := false
]

{ #category : #accessing }
ASCBlock >> manifest [

^	manifest
]

{ #category : #serializing }
ASCBlock >> manifest: aGenerator [

	^ manifest ifFalse: [
		manifest := true.
		aGenerator manifest: self ]
]

{ #category : #'code generation' }
ASCBlock >> outputTo: output [

	^ output visitPushBlock: self
]

{ #category : #'instance creation' }
ASCBlock >> with: anASBlock [

	block := anASBlock
]
