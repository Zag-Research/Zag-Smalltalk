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
ASCBasicBlockTest >> testStackFlow [

	| bb1 bb2 bb3 bb4 cg |
	cg := output codeGenerator.
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
	bb3 modifies: #l1.
	bb2 needs: #p1.
	bb4 needs: #p1.
	bb4 needs: #self.
	bb4 needs: #l1.
	cg resolveFlowDependencies.
	self assert: (bb2 provides includes: #p1).
	self assert: (bb3 provides includes: #p1).
	self deny: (bb1 provides includes: #l1).
	self assert: (bb3 provides: #l1).
	self assert: (bb2 provides: #l1).
	self assert: (bb4 requires includes: #p1).
	cg inspect
]

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
