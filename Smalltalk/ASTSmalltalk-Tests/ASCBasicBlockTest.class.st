Class {
	#name : 'ASCBasicBlockTest',
	#superclass : 'TestCase',
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
ASCBasicBlockTest >> testNameStack [

	| bb |
	bb := ASCInlineBlock new.
	bb doPush: 42.
	bb doPush: 27.
	bb doPush: 98.
	bb nameStack: #( p1 p2 ).
	bb doPush: 17.
	bb doPush: 127.
	self assert: (bb at: #p1) value equals: 27.
	self assert: (bb at: #p2) value equals: 98.
	self assert: (bb at: #self) value equals: 42.
	self assert: (bb at: 0) value equals: 127.
	self assert: (bb at: 1) value equals: 17
]

{ #category : 'tests' }
ASCBasicBlockTest >> testStackFlow [

	| bb1 bb2 bb3 bb4 cg bb3Rl1 bb3Sl1 |
	cg := ASCodeGenerator new.
	cg targetClass: SmallInteger.
	cg selector: #foo:bar:.
	cg locals: #( l1 l2 ) parameters: #( p1 p2 ).
	bb1 := cg firstBlock.
	self assert: (bb1 at: #l1) source isNilReference.
	self assert: (bb1 at: #l1) offset > (bb1 at: #l2) offset.
	self assert: (bb1 at: #p1) offset > (bb1 at: #p2) offset.
	bb2 := cg newInline.
	bb3 := cg newInline.
	bb4 := cg newInline.
	bb2 addSourceBlock: bb1.
	bb3 addSourceBlock: bb1.
	bb4 addSourceBlock: bb2.
	self assert: (bb1 at: #l1) source isNilReference.
	self deny: (bb2 at: #l1) equals: (bb1 at: #l1).
	bb4 addSourceBlock: bb3.
	self assert: (bb1 at: #l1) source isNilReference.
	bb3Rl1 := bb3 reference: #l1.
	self assert: (bb1 at: #l1) source isNilReference.
	self assert: (bb1 at: #l1) source isNilReference.
	bb3Sl1 := bb3 storeInto: #l1.
	self assert: (bb1 at: #l1) source isNilReference.
	bb4 reference: #p1.
	bb4 reference: #self.
	self assert: (bb1 at: #l1) source isNilReference.
	bb4 reference: #l1.
	self assert: (bb1 at: #l1) source isNilReference.
	cg resolveFlowDependencies.
	self assert: bb4 namedBlock equals: bb1.
	self assert: (bb1 at: #l1) source isNilReference.
	self assert: (bb2 at: #p1) equals: (bb3 at: #p1).
	self assert: (bb1 at: #p1) source isReferenced.
	self deny: (bb1 at: #p2) source isReferenced.
	self assert: (bb4 at: #l1) source isPhiReference.
	self assert: (bb1 at: #l2) source isNilReference.
	self assert: (bb2 at: #l2) source isNilReference.
	self assert: (bb4 at: #l2) source isNilReference.
	cg inspect
]

{ #category : 'tests' }
ASCBasicBlockTest >> test_extend [

	"1. push temp values and convert to named values"
	"2. test for scope"
]

{ #category : 'tests' }
ASCBasicBlockTest >> test_plus_and_and [
	"plus: n1 and: n2 and: n3

	^ n1 + (n2 * n3)"

	| cg |
	cg := ASCodeGenerator new.
	cg targetClass: ASCompileTestClass1.
	cg selector: #plus:and:and:.
	cg locals: #(  ) parameters: #( n1 n2 n3 ).
	cg requireContext.
	cg pushVariable: #n1.
	cg pushVariable: #n2.
	cg pushVariable: #n3.
	cg send: #*. " and create new ReturnBlock"
	cg send: #+. " and create new ReturnBlock"
	cg returnTop.
	cg resolveFlowDependencies.
	cg inspect
]
