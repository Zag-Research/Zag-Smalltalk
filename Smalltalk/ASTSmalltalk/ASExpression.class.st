"
I have subclasses for all the expressions that can exist.

Note that ASBlock is also an ASExpression although the class taxonomy doesn't reflect it.
"
Class {
	#name : 'ASExpression',
	#superclass : 'ASStatement',
	#category : 'ASTSmalltalk-AST',
	#package : 'ASTSmalltalk',
	#tag : 'AST'
}

{ #category : 'visiting' }
ASExpression >> acceptVisitor: aVisitor [
	self subclassResponsibility 
]

{ #category : 'compiling' }
ASExpression >> tell: aCompiler youAreSent: aSymbol [

	^ aCompiler sentToUnknownObject: aSymbol
]

{ #category : 'compiling' }
ASExpression >> zigWalk: aGenerator [

	self subclassResponsibility 
]

{ #category : 'compiling' }
ASExpression >> zigWalkBlockReturn: aGenerator [

	self zigWalk: aGenerator.
	^ aGenerator returnTop
]

{ #category : 'compiling' }
ASExpression >> zigWalkDiscard: aGenerator [

	self zigWalk: aGenerator.
	^ aGenerator drop
]

{ #category : 'compiling' }
ASExpression >> zigWalkForReturn: aGenerator [

	self zigWalk: aGenerator.
	^ false
]

{ #category : 'compiling' }
ASExpression >> zigWalkMethodReturn: aGenerator [

	self zigWalkDiscard: aGenerator.
	^ aGenerator returnSelf
]

{ #category : 'compiling' }
ASExpression >> zigWalkTarget: aGenerator [ 
	self zigWalk: aGenerator.
	^ nil
]
