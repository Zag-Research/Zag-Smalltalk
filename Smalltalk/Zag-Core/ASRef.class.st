"
I represent a reference to a variable (local, parameters, inherited local/parameters, instance, class, shared).
"
Class {
	#name : 'ASRef',
	#superclass : 'ASExpression',
	#instVars : [
		'variable'
	],
	#category : 'Zag-Core-AST',
	#package : 'Zag-Core',
	#tag : 'AST'
}

{ #category : 'instance creation' }
ASRef class >> variable: aSymbol [
^ self new variable: aSymbol
]

{ #category : 'comparing' }
ASRef >> = other [

	^ self class = other class and: [ self variable = other variable ]
]

{ #category : 'printing' }
ASRef >> inspectZagASTOn: aStream [
	super inspectZagASTOn: aStream.
	aStream << $( << variable << $)
]

{ #category : 'comparing' }
ASRef >> isASRef [
	^true
]

{ #category : 'testing' }
ASRef >> isImmediateThunk [
^ variable isImmediateThunk
]

{ #category : 'comparing' }
ASRef >> variable [

	^ variable
]

{ #category : 'comparing' }
ASRef >> variable: aSymbol [
	aSymbol isSymbol ifFalse: [ self halt ].
	variable := aSymbol 
]

{ #category : 'compiling' }
ASRef >> zigWalk: aGenerator [

	^ aGenerator pushVariable: variable
]
