Class {
	#name : 'ASCBasicBlockTest',
	#superclass : 'TestCase',
	#instVars : [
		'blocks',
		'bb1',
		'bb2',
		'bb5',
		'bb3',
		'bb6',
		'bb4',
		'bb7',
		'bb8'
	],
	#category : 'Zag-Core-Tests-Tests',
	#package : 'Zag-Core-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
ASCBasicBlockTest >> setUp [

	super setUp.

	self halt.
	bb1 := ASCCompiledMethodBlock new
		       label: 'bb1';
		       selector: #main;
		       locals: #(  ) parameters: #(  ).
	bb1 pushLiteral: 1.
	bb2 := (bb1 send: #foo targetClass: nil tail: false) label: 'bb2'.
	bb3 := (bb2 send: #foo targetClass: nil tail: false) label: 'bb3'
]

{ #category : 'running' }
ASCBasicBlockTest >> setup8graph [

	| bbs methods |
	bb3 branch: bb2.
	methods := {
		           (ASMethodNode new
			            selector: #foo;
			            methodClass: True).
		           (ASMethodNode new
			            selector: #foo;
			            methodClass: False) }.
	bbs := (bb2 classCase: methods exhaustive: true genertor: nil)
		       nextBasicBlocks.
	bb4 := bbs first
		       label: 'bb4';
		       returnEmpty.
	bb5 := bbs second label: 'bb5'.
	bb5 pushLiteral: 2.
	bb6 := (bb5 send: #foo targetClass: nil tail: false) label: 'bb6'.
	bbs := (bb5 classCase: methods exhaustive: true genertor: nil)
		       nextBasicBlocks.
	bb7 := bbs first label: 'bb7'.
	bb8 := bbs second label: 'bb8'.
	bb8 returnTop.
	bb1
		inspect;
		halt
]

{ #category : 'tests' }
ASCBasicBlockTest >> testAddBasicBlock [

	| aBB |
	aBB := ASCBasicBlock new.
	bb1 addSourceBlock: aBB.

	self assert: bb1 blocks size equals: 1
]

{ #category : 'tests' }
ASCBasicBlockTest >> testAfterSources [

	| count |
	count := 0.
	self halt.
	bb3 returnFinal.
	bb1 afterSourcesDo: [ :bb | count := count + 1 ].
	self assert: count equals: 8
]

{ #category : 'tests' }
ASCBasicBlockTest >> testAfterSources8 [

	| count |
	self setup8graph.
	count := 0.
	self halt.
	blocks first afterSourcesDo: [ :bb | count := count + 1 ].
	self assert: count equals: 8
]

{ #category : 'tests' }
ASCBasicBlockTest >> testAfterSourcesOrdered [

	| labels |
	labels := OrderedCollection new.
	self halt.
	bb1 afterSourcesDo: [ :bb | labels add: bb label asSymbol ].
	self assert: labels equals: #( bb1 bb4 bb5 bb8 bb7 bb6 bb3 bb2).
]

{ #category : 'tests' }
ASCBasicBlockTest >> testNameStack [

	| bb |
	bb := ASCJoinBlock new.
	bb doPush: 42.
	bb doPush: 27.
	bb doPush: 98.
	bb nameStack: #( p1 p2 ).
	bb doPush: 17.
	bb doPush: 127.
	self assert: (bb at: #p1) data equals: (AStSmallInteger setValue: 27).
	self assert: (bb at: #p2) data equals: (AStSmallInteger setValue: 98).
	self assert: (bb at: #self) data equals: (AStSmallInteger setValue: 42).
	self assert: (bb at: 0) data equals: (AStSmallInteger setValue: 127).
	self assert: (bb at: 1) data equals: (AStSmallInteger setValue: 17).
]

{ #category : 'tests' }
ASCBasicBlockTest >> testStackFlow [

	| bb1 bb2 bb3 bb4 cg bb3Rl1 bb3Sl1 |
	cg := ASCodeGenerator new.
	cg targetClass: SmallInteger.
	cg selector: #foo:bar:.
	cg locals: #( l1 l2 ) parameters: #( p1 p2 ).

	bb1 := cg methodBlock.
	self assert: (bb1 at: #l1) data isNilReference.
	self assert: (bb1 at: #l1) offset > (bb1 at: #l2) offset.
	self assert: (bb1 at: #p1) offset > (bb1 at: #p2) offset.

	bb2 := cg newInline.
	bb3 := cg newInline.
	bb4 := cg newInline.
	bb2 branchedToFrom: bb1.
	bb3 branchedToFrom: bb1.
	bb4 branchedToFrom: bb2.

	self assert: (bb1 at: #l1) data isNilReference.
	self deny: (bb2 at: #l1) equals: (bb1 at: #l1).

	bb4 branchedToFrom: bb3.
	self assert: (bb1 at: #l1) data isNilReference.

	bb3Rl1 := bb3 reference: #l1.
	self assert: (bb1 at: #l1) data isNilReference.
	self assert: (bb1 at: #l1) data isNilReference.

	bb3Sl1 := bb3 storeInto: #l1.
	self assert: (bb1 at: #l1) data isNilReference.

	bb4 reference: #p1.
	bb4 reference: #self.
	self assert: (bb1 at: #l1) data isNilReference.

	bb4 reference: #l1.
	self assert: (bb1 at: #l1) data isNilReference.

	cg
		finalAnalysis;
		flowAnalysis.
	self assert: bb4 namedBlock equals: bb1.

	self assert: (bb1 at: #l1) data isNilReference.
	self assert: (bb2 at: #p1) equals: (bb3 at: #p1).
	self assert: (bb1 at: #p1) data isReferenced.
	self deny: (bb1 at: #p2) data isReferenced.
	self assert: (bb4 at: #l1) data isPhiReference.
	self assert: (bb1 at: #l2) data isNilReference.
	self assert: (bb2 at: #l2) data isNilReference.
	self assert: (bb4 at: #l2) data isNilReference.
	cg inspect
]

{ #category : 'tests' }
ASCBasicBlockTest >> test_extend [
	"1. push temp values and convert to named values"

	| bb block ascBlock |
	bb := ASCJoinBlock new.
	bb doPush: 42.
	bb doPush: 27.
	bb doPush: 98.
	bb nameStack: #( p1 p2 ).
	"2. test for scope, ASCBlocks have a scope attribute which points to a stack"
	"who references ASCBlocks?"
	block := ASCBlockClosureValue new.
	block name: #testBlock.
	ascBlock := ASCBlock with: block.
	bb setStackForBlock: ascBlock.
	self assert: (ascBlock scope at: #p1) id equals: (bb at: #p1) id
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
	cg pushVariable: #n1.
	cg pushVariable: #n2.
	cg pushVariable: #n3.
	cg send: #*. " and create new ReturnBlock"
	cg send: #+. " and create new ReturnBlock"
	cg returnTop.
	cg
		finalAnalysis;
		flowAnalysis.
	cg inspect
]
