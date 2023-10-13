Class {
	#name : #ASTBlock,
	#superclass : #Object,
	#instVars : [
		'blockNumber',
		'sequence'
	],
	#category : #'ASTSmalltalk-IR'
}

{ #category : #initializing }
ASTBlock >> from: anIRSequence [

	blockNumber := anIRSequence orderNumber.
	sequence := anIRSequence collect: [ :node | node asAST ]
]
