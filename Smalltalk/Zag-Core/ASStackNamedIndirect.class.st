"
I am a named stack variable but am found in a parents stack.
"
Class {
	#name : 'ASStackNamedIndirect',
	#superclass : 'ASParameterNamed',
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

{ #category : 'as yet unclassified' }
ASStackNamedIndirect >> indirectElement [

	^ self
]

{ #category : 'accessing' }
ASStackNamedIndirect >> myScope: aStackContext [

	myScope := aStackContext
]

{ #category : 'as yet unclassified' }
ASStackNamedIndirect >> offsetToSelf: anInteger [

	self halt
]
