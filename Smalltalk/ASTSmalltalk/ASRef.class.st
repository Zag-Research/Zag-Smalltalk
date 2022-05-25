"
I represent a reference to a variable (local, parameters, inherited local/parameters, instance, class, shared).
"
Class {
	#name : #ASRef,
	#superclass : #ASExpression,
	#instVars : [
		'name'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #'instance creation' }
ASRef class >> variable: aSymbol [
^ self new variable: aSymbol
]

{ #category : #comparing }
ASRef >> = other [

	^ self class = other class and: [ self variable = other variable ]
]

{ #category : #comparing }
ASRef >> name [

	^ name
]

{ #category : #comparing }
ASRef >> name: aSymbol [

	name := aSymbol
]

{ #category : #printing }
ASRef >> printOn: s [

	s << self className << ' variable: '.
	name printOn: s
]
