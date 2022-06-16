"
I represent a variable (local, parameters, inherited local/parameters, instance, class, shared).
"
Class {
	#name : #ASVariable,
	#superclass : #Object,
	#instVars : [
		'name'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #'reflective operations' }
ASVariable class >> doesNotUnderstand: aMessage [

	aMessage selector numArgs > 0 ifTrue: [ 
		^ super doesNotUnderstand: aMessage ].
	^ self name: aMessage selector
]

{ #category : #'instance creation' }
ASVariable class >> name: aString [ 
	^ self new name: aString
]

{ #category : #comparing }
ASVariable >> = other [

	^ self class = other class and: [ self name = other name ]
]

{ #category : #comparing }
ASVariable >> name [

	^ name
]

{ #category : #comparing }
ASVariable >> name: aSymbol [

	name := aSymbol
]

{ #category : #printing }
ASVariable >> printOn: s [

	s << self className << ' ' <<
	name 
]

{ #category : #compiling }
ASVariable >> tell: aCompiler youAreSent: aSymbol [

	^ aCompiler sentToUnknownObject: aSymbol
]
