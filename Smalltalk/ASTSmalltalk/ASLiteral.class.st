"
I represent any literal value.
"
Class {
	#name : #ASLiteral,
	#superclass : #ASExpression,
	#instVars : [
		'literal'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #'instance creation' }
ASLiteral class >> literal: aLiteralObject [
	^ self new literal: aLiteralObject
]

{ #category : #comparing }
ASLiteral >> = other [

	^ self class = other class and: [ self literal = other literal ]
]

{ #category : #visiting }
ASLiteral >> acceptVisitor: aVisitor [

	^ aVisitor visitLiteral: self
]

{ #category : #'as yet unclassified' }
ASLiteral >> baseValue [
	^ literal
]

{ #category : #accessing }
ASLiteral >> isASLiteral [
	^true
]

{ #category : #accessing }
ASLiteral >> literal [

	^ literal
]

{ #category : #accessing }
ASLiteral >> literal: aLiteralObject [

	literal := aLiteralObject
]

{ #category : #printing }
ASLiteral >> printOn: s [

	s << self className << ' literal: '.
	literal printOn: s
]

{ #category : #compiling }
ASLiteral >> tell: aCompiler youAreSent: aSymbol [

	^ aCompiler sent: aSymbol toLiteral: literal
]

{ #category : #accessing }
ASLiteral >> zigString: syms and: stackVariables [
^ '&e.pushLiteral, Object.from(' , literal asString , '),'
]
