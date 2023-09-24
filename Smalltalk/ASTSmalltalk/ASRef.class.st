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

{ #category : #'instance creation' }
ASRef class >> variable: aSymbol [
^ self new variable: aSymbol
]

{ #category : #comparing }
ASRef >> = other [

	^ self class = other class and: [ self variable = other variable ]
]

{ #category : #visiting }
ASRef >> acceptVisitor: aVisitor [
	^ aVisitor visitVariable: self
]

{ #category : #converting }
ASRef >> asZagCompileObject [
	^ variable asZagCompileObject 
]

{ #category : #comparing }
ASRef >> asZagLabel [ 
 ^ variable asZagLabel 
]

{ #category : #comparing }
ASRef >> isASRef [
	^true
]

{ #category : #printing }
ASRef >> printOn: s [

	s << self className << ' variable: ('.
	variable printOn: s.
	s << ')'
]

{ #category : #comparing }
ASRef >> variable [

	^ variable
]

{ #category : #comparing }
ASRef >> variable: aSymbol [

	variable := aSymbol
]

{ #category : #comparing }
ASRef >> zigString: syms and: stackVariables [
	"load value of reference to the stack? where do I get the value from? local, global arg, instance "
^ '&e.pushLocal, ', (stackVariables indexOf: variable name) asString,','.
]
