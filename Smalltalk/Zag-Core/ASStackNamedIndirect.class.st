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

	self halt.
	"Dave I'm not sure what referenceVariable is suppose to do."
	"^ (myScope at: aSymbol) referenceVariable: self"
	^ myScope at: aSymbol
]

{ #category : 'instance creation' }
ASStackNamedIndirect >> class: aClass [

	self myScope: (ZagScope new class: aClass)
]

{ #category : 'as yet unclassified' }
ASStackNamedIndirect >> indirectElement [

	^ self
]

{ #category : 'accessing' }
ASStackNamedIndirect >> myScope: aScope [

	myScope := aScope
]

{ #category : 'as yet unclassified' }
ASStackNamedIndirect >> offsetToSelf: anInteger [

	self halt
]
