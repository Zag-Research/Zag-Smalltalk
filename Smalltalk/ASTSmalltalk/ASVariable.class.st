"
I represent a variable (local, parameters, inherited local/parameters, instance, class, shared).
"
Class {
	#name : #ASVariable,
	#superclass : #Object,
	#instVars : [
		'name',
		'referencedBy',
		'modifiedBy'
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

{ #category : #accessing }
ASVariable >> hasModifiers [

	^modifiedBy isNotEmpty
]

{ #category : #testing }
ASVariable >> initialize [ 
	referencedBy := Set new.
	modifiedBy := Set new
]

{ #category : #testing }
ASVariable >> isASArg [
	^false
]

{ #category : #comparing }
ASVariable >> isASBlock [

	^ false
]

{ #category : #accessing }
ASVariable >> modifiedBy: anASBlock [ 
	modifiedBy add: anASBlock
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

{ #category : #visiting }
ASVariable >> referencedBy: anASBlock [ 
	referencedBy add: anASBlock 
]

{ #category : #accessing }
ASVariable >> references [
	^referencedBy 
]

{ #category : #compiling }
ASVariable >> tell: aCompiler youAreSent: aSymbol [

	^ aCompiler sentToUnknownObject: aSymbol
]

{ #category : #building }
ASVariable >> zigString: syms and: stackVariables [
	"The argument is on the stack already so should not need to push it"

	^ '&e.pushLocal, ' , ((stackVariables indexOf: name) - 1) asString
	  , ', '
]
