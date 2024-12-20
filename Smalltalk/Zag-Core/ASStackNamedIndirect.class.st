"
I am a named stack variable but am found in a parents stack.
"
Class {
	#name : 'ASStackNamedIndirect',
	#superclass : 'ASStackSelf',
	#instVars : [
		'myScope'
	],
	#category : 'Zag-Core-Stack',
	#package : 'Zag-Core',
	#tag : 'Stack'
}

{ #category : 'accessing' }
ASStackNamedIndirect >> at: aSymbol [

	^ myScope at: aSymbol
]

{ #category : 'accessing' }
ASStackNamedIndirect >> myScope: aStackContext [

	myScope := aStackContext
]

{ #category : 'as yet unclassified' }
ASStackNamedIndirect >> offsetToSelf: anInteger [

	self halt
]
