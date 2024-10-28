Class {
	#name : 'ASRuntimeTest',
	#superclass : 'TestCase',
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
ASRuntimeTest >> setUp [
	super setUp.

	"Put here a common initialization logic for tests"
]

{ #category : 'tests' }
ASRuntimeTest >> testHowManyTimesCalled [

	| x |
	x := ASCompileTestClass3 new.
	self assert: (x aCallsB: 0) equals: 20
]

{ #category : 'tests' }
ASRuntimeTest >> testHowManyTimesCalled2 [

	| x |
	x := ASCompileTestClass3 new.
	self assert: (x aCallsB) equals: 20
]
