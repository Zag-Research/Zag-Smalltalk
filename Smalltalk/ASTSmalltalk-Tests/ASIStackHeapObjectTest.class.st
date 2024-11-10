Class {
	#name : 'ASIStackHeapObjectTest',
	#superclass : 'TestCase',
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
ASIStackHeapObjectTest >> testSimple [

	| it |
	it := ASIStackHeapObject new array: #( 1 2 3 4 5 ) offset: 2 size: 3.
	self assert: (it at: 0) equals: 2.
	self assert: (it at: 2) equals: 4.
	self assert: it isOnStack.
	it toHeap.
	self assert: (it at: 0) equals: 2.
	self assert: (it at: 2) equals: 4.
	it at: 2 put: 7.
	self assert: (it at: 2) equals: 7.
	self deny: it isOnStack
]
