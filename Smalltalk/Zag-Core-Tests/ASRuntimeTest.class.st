Class {
	#name : 'ASRuntimeTest',
	#superclass : 'TestCase',
	#category : 'Zag-Core-Tests-Tests',
	#package : 'Zag-Core-Tests',
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
