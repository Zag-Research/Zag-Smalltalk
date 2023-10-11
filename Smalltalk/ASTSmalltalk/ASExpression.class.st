"
I have subclasses for all the expressions that can exist.

Note that ASBlock is also an ASExpression although the class taxonomy doesn't reflect it.
"
Class {
	#name : #ASExpression,
	#superclass : #ASStatement,
	#traits : 'ASValue',
	#classTraits : 'ASValue classTrait',
	#category : #'ASTSmalltalk-AST'
}

{ #category : #visiting }
ASExpression >> acceptVisitor: aVisitor [
	self subclassResponsibility 
]

{ #category : #compiling }
ASExpression >> tell: aCompiler youAreSent: aSymbol [

	^ aCompiler sentToUnknownObject: aSymbol
]

{ #category : #compiling }
ASExpression >> zigWalk: aGenerator [

	self subclassResponsibility 
]

{ #category : #compiling }
ASExpression >> zigWalkForReturn: aGenerator [
	^ self zigWalk: aGenerator
]

{ #category : #compiling }
ASExpression >> zigWalkTarget: aGenerator [ 
	self zigWalk: aGenerator.
	^ nil
]
