"
I am a named stack variable but am found in a parents stack.
"
Class {
	#name : 'ASStackNamedIndirect',
	#superclass : 'ASStackSelf',
	#instVars : [
		'myScope'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'accessing' }
ASStackNamedIndirect >> at: aSymbol [

	^ myScope at: aSymbol
]

{ #category : 'as yet unclassified' }
ASStackNamedIndirect >> offsetToSelf: anInteger [

	self halt
]

{ #category : 'accessing' }
ASStackNamedIndirect >> value: anObject [

	super value: anObject.
	myScope := anObject block scope
]
