Class {
	#name : 'ASCBasicBlockTest',
	#superclass : 'ASLLVMTests',
	#instVars : [
		'output'
	],
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
ASCBasicBlockTest >> setUp [

	super setUp.

	"Create new instance of generator and set testing inst. var"
	output := ASLLMethodOutput new context: file "testing"
]

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

	| bb1 bb2 bb3 bb4 cg |
	cg := ASCodeGenerator new.
	cg targetClass: SmallInteger.
	cg selector: #foo:bar:.
	cg locals: #( l1 l2 ) parameters: #( p1 p2 ).
	bb1 := cg firstBlock.
	bb2 := cg newInline.
	bb3 := cg newInline.
	bb4 := cg newInline.
	self assert: bb4 namedMethod equals: bb1.
	bb2 addSource: bb1.
	bb3 addSource: bb1.
	bb4 addSource: bb2.
	bb4 addSource: bb3.
	bb3 storeInto: #l1.
	bb2 reference: #l1.
	bb2 reference: #p1.
	bb4 reference: #p1.
	bb4 reference: #self.
	bb4 reference: #l1.
	cg resolveFlowDependencies.
	self assert: (bb2 provides: #p1).
	self assert: (bb3 provides: #p1).
	self deny: (bb1 provides: #l1).
	self assert: (bb3 provides: #l1).
	self assert: (bb2 provides: #l1).
	self assert: (bb4 requires includes: #p1).
	cg inspect
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
	cg makeContextRequired.
	cg pushVariable: #n1.
	cg pushVariable: #n2.
	cg pushVariable: #n3.
	cg send: #*. " and create new RetutnBlock"
	cg send: #+. " and create new RetutnBlock"
	cg returnTop.
	cg inspect
]
