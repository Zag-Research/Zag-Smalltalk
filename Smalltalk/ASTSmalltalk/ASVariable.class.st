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

{ #category : #testing }
ASVariable >> isASArg [
	^false
]

{ #category : #comparing }
ASVariable >> isASBlock [

	^ false
]

{ #category : #comparing }
ASVariable >> name [

	^ name
]

{ #category : #comparing }
ASVariable >> name: aSymbol [

	name := aSymbol
]

{ #category : #affecting }
ASVariable >> pop: output [
^ self subclassResponsibility 
]

{ #category : #printing }
ASVariable >> printOn: s [

	s << self className << ' ' <<
	name 
]

{ #category : #affecting }
ASVariable >> push: output [

	^ self subclassResponsibility
]

{ #category : #affecting }
ASVariable >> store: output [

	^ self subclassResponsibility
]

{ #category : #compiling }
ASVariable >> tell: aCompiler youAreSent: aSymbol [

	^ aCompiler sentToUnknownObject: aSymbol
]

{ #category : #accessing }
ASVariable >> variable [

	^ self shouldBeImplemented
]
