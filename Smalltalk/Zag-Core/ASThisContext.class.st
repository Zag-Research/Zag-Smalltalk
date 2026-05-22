"
I represent a reference to `thisContext`
"
Class {
	#name : 'ASThisContext',
	#superclass : 'ASConstant',
	#category : 'Zag-Core-AST',
	#package : 'Zag-Core',
	#tag : 'AST'
}

{ #category : 'converting' }
ASThisContext >> asPharoAST [
	^OCVariableNode thisContextNode.
]

{ #category : 'compiling' }
ASThisContext >> zigWalk: aGenerator [

	self error
]
