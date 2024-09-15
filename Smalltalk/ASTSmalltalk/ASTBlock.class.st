"
I represent a block closure object on the stack.

I can only represent myself, so even another block closure isn't compatible.
"
Class {
	#name : 'AStBlock',
	#superclass : 'AStackValue',
	#instVars : [
		'block'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'accessing' }
AStBlock >> block: anASBlock [

	block := anASBlock
]

{ #category : 'as yet unclassified' }
AStBlock >> couldMergeIn: other [

	^ self == other
]

{ #category : 'as yet unclassified' }
AStBlock >> dataClass [

	^ BlockClosure
]

{ #category : 'as yet unclassified' }
AStBlock >> finalAnalysis [

	self halt
]
