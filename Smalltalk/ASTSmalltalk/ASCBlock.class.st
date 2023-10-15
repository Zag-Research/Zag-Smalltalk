"
I hold a ASBlock AST, to prepare for analysis
"
Class {
	#name : #ASCBlock,
	#superclass : #ASCOperations,
	#instVars : [
		'block',
		'compiledBlock'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #accessing }
ASCBlock >> block [
	^ block
]

{ #category : #'code generation' }
ASCBlock >> outputTo: output [

	output visitPushBlock: block
]

{ #category : #'instance creation' }
ASCBlock >> with: anASBlock [

	block := anASBlock
]
