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

{ #category : 'instance creation' }
ASStackNamedIndirect >> class: aClass [

	self myScope: (ZagScope new class: aClass)
]

{ #category : 'accessing' }
ASStackNamedIndirect >> countParameters [

	^ myScope next countParameters 
]

{ #category : 'displaying' }
ASStackNamedIndirect >> displayStringOn: stream [
	stream << #ASStackNamedIndirect 
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
