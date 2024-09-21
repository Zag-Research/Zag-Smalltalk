Class {
	#name : 'ASCValueTest',
	#superclass : 'TestCase',
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
ASCValueTest >> testBoolean [

	| val |
	self fail: 'needs rewrite'.
	val := AStExactClass new store: (ASCLiteralValue new init: true).
	self assert: val kind equals: True.
	val store: (ASCLiteralValue new init: false).
	self assert: val kind equals: Boolean.
	self deny: val isRanged
]

{ #category : 'tests' }
ASCValueTest >> testNumbers [

	| val |
	self fail: 'needs rewrite'.
	val := AStExactClass new store: (ASCLiteralValue new init: 17).
	self assert: val kind equals: SmallInteger.
	val store: (ASCLiteralValue new init: 42).
	self assert: val kind equals: SmallInteger.
	self assert: val isRanged.
	self assert: val min equals: 17.
	self assert: val max equals: 42.
	val store: (ASCLiteralValue new init: -5).
	self assert: val min equals: -5
]
