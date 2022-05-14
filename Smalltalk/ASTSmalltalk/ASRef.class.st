"
I represent a reference to a variable (local, parameters, inherited local/parameters, instance, class, shared).
"
Class {
	#name : #ASRef,
	#superclass : #ASExpression,
	#instVars : [
		'variable'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #comparing }
ASRef >> = other [

	^ self class = other class and: [ self variable = other variable ]
]

{ #category : #printing }
ASRef >> printOn: s [

	s << self className << ' new variable: '.
	variable printOn: s
]

{ #category : #comparing }
ASRef >> variable [
	^ variable
]

{ #category : #comparing }
ASRef >> variable: aSymbol [

	variable := aSymbol
]
