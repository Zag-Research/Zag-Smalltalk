"
I am is a test class for testing the behavior of `ASemanticInterpreter` and its related classes
"
Class {
	#name : 'ASemanticInterpreterTest',
	#superclass : 'TestCase',
	#instVars : [
		'interpreter'
	],
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'as yet unclassified' }
ASemanticInterpreterTest >> primitiveSuccess: anObject [

	^ anObject
]

{ #category : 'running' }
ASemanticInterpreterTest >> setUp [

	super setUp.

	interpreter := ASemanticInterpreter new
]

{ #category : 'tests' }
ASemanticInterpreterTest >> testPrimitive [

	self assert: ((1 siPrim1: 2) processPrimitiveResult: self) equals: 3
]

{ #category : 'tests' }
ASemanticInterpreterTest >> testRestructureStack [

	1 to: 10 do: [ :n | interpreter push: n ].
	interpreter restructureStack: 16r101.
	self assert: interpreter stackDepth equals: 10.
	self assert: interpreter top equals: 10.
	interpreter restructureStack: 16r205.
	self assert: interpreter stackDepth equals: 6.
	self assert: interpreter top equals: 9.
	interpreter restructureStack: 16r010204.
	self assert: interpreter stackDepth equals: 4.
	self assert: (interpreter top: 3) equals: #( 9 5 2 ).
	interpreter restructureStack: 16r020102.
	self assert: interpreter stackDepth equals: 4.
	self assert: (interpreter top: 3) equals: #( 5 9 2 )
]

{ #category : 'tests' }
ASemanticInterpreterTest >> testSimple [

	| it |
	it := ASIStackHeapObject new array: #( 1 2 3 4 5 ) offset: 1 size: 3.
	self assert: (it at: 1) equals: 2.
	self assert: (it at: 3) equals: 4.
	self assert: it isOnStack.
	it toHeap.
	self assert: (it at: 1) equals: 2.
	self assert: (it at: 3) equals: 4.
	it at: 2 put: 7.
	self assert: (it at: 2) equals: 7.
	self deny: it isOnStack
]
