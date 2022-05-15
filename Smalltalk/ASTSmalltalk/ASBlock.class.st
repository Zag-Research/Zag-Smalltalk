"
I represent a BlockClosure which like a Method except that returns return from the enclosing method and self refers to the object of the enclosing method.

Note that this is also an ASExpression although the class taxonomy doesn't reflect it.
"
Class {
	#name : #ASBlock,
	#superclass : #ASMethodOrBlock,
	#instVars : [
		'method'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #printing }
ASBlock >> printOn: s [

	s << self className << ' new yourself'.
	super printOn: s
]
