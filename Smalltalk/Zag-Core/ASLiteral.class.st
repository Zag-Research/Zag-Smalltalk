"
I represent any literal value.
"
Class {
	#name : 'ASLiteral',
	#superclass : 'ASExpression',
	#instVars : [
		'literal'
	],
	#category : 'Zag-Core-AST',
	#package : 'Zag-Core',
	#tag : 'AST'
}

{ #category : 'instance creation' }
ASLiteral class >> literal: aLiteralObject [
	^ self new literal: aLiteralObject
]

{ #category : 'comparing' }
ASLiteral >> = other [

	^ self class = other class and: [ self literal = other literal ]
]

{ #category : 'visiting' }
ASLiteral >> accept: aVisitor [

	^ aVisitor visitLiteral: literal
]

{ #category : 'accessing' }
ASLiteral >> inferType: aClass [

	^ literal class
]

{ #category : 'printing' }
ASLiteral >> inspectZagASTOn: aStream [
	super inspectZagASTOn: aStream.
	aStream << $(.
	aStream print: literal.
	aStream << $)
]

{ #category : 'accessing' }
ASLiteral >> isASLiteral [
	^true
]

{ #category : 'testing' }
ASLiteral >> isImmediateThunk [
^ true
]

{ #category : 'testing' }
ASLiteral >> isSimple [
	^ literal = true or: (literal = false or: (literal = 1 or: (literal = -1
	  or: (literal = 0 or: (literal = 2)))))
]

{ #category : 'accessing' }
ASLiteral >> literal [

	^ literal
]

{ #category : 'accessing' }
ASLiteral >> literal: aLiteralObject [

	literal := aLiteralObject
]

{ #category : 'compiling' }
ASLiteral >> tell: aCompiler youAreSent: aSymbol [

	^ aCompiler sent: aSymbol toLiteral: literal
]

{ #category : 'accessing' }
ASLiteral >> with: anObject [ 

	literal := anObject
]

{ #category : 'compiling' }
ASLiteral >> zigWalk: aGenerator [

	^ aGenerator pushLiteral: literal
]
