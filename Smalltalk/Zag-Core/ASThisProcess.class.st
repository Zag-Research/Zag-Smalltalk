"
I represent a reference to `thisProcess`
"
Class {
	#name : 'ASThisProcess',
	#superclass : 'ASConstant',
	#category : 'Zag-Core-AST',
	#package : 'Zag-Core',
	#tag : 'AST'
}

{ #category : 'visiting' }
ASThisProcess >> accept: aVisitor [
	^aVisitor visitThisProcess: self
]

{ #category : 'converting' }
ASThisProcess >> asPharoAST [
	^OCVariableNode thisProcessNode.
]

{ #category : 'compiling' }
ASThisProcess >> zigWalk: aGenerator [

	self error
]
