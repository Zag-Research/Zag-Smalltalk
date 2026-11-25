"
I represent a reference to a variable. This pushes the value of a variable (local, parameters, inherited local/parameters, , instance, class, shared).

My variables are:
- #variable is a symbol containing the name of the variable I represent.
- #type is an optional class that is the type of my value.
"
Class {
	#name : 'ASRef',
	#superclass : 'ASExpression',
	#instVars : [
		'variable',
		'type'
	],
	#category : 'Zag-Core-AST',
	#package : 'Zag-Core',
	#tag : 'AST'
}

{ #category : 'instance creation' }
ASRef class >> variable: aSymbol [
^ self new variable: aSymbol
]

{ #category : 'instance creation' }
ASRef class >> variable: aSymbol type: aClass [
	^self new variable: aSymbol; type: aClass; yourself
]

{ #category : 'comparing' }
ASRef >> = other [

	^ self class = other class and: [ self variable = other variable and: [ self type = other type ] ]
]

{ #category : 'visiting' }
ASRef >> accept: aVisitor [
	^aVisitor visitRef: self
]

{ #category : 'converting' }
ASRef >> asPharoAST [
	^OCVariableNode named: variable.
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

{ #category : 'accessing' }
ASRef >> type [
	"My type.  May be nil to represent no type being specified."
	^ type
]

{ #category : 'accessing' }
ASRef >> type: anObject [

	type := anObject
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
