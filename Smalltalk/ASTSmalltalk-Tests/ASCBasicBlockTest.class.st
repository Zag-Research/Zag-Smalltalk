Class {
	#name : 'ASCBasicBlockTest',
	#superclass : 'TestCase',
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
ASCBasicBlockTest >> testStackNames [

	| bb |
	bb := ASCInlineBlock new.
	bb doPush: 42.
	bb doPush: 27.
	bb doPush: 98.
	bb nameStack: #( p1 p2 ).
	self assert: (bb stack at: #p1) equals: 27.
	self assert: (bb stack at: #p2) equals: 98.
	self assert: (bb stack at: #self) equals: 42
]
