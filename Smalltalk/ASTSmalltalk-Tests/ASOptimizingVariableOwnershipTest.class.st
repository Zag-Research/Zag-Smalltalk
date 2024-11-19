"
I test algorithm that optimizes the ownership of variables in blocks. See https://github.com/Zag-Research/Zag-Smalltalk/edit/main/Documentation/Final%20Analysis.md#optimizing-local-variable-locations for more information.
"
Class {
	#name : 'ASOptimizingVariableOwnershipTest',
	#superclass : 'TestCase',
	#instVars : [
		'cg',
		'stack'
	],
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
ASOptimizingVariableOwnershipTest >> setUp [

	| first |
	super setUp.

	"Put here a common initialization logic for tests"
	cg := ASCodeGenerator new.
	first := cg basicBlocks first.
	first label: 'test method'.
	stack := first stack
]

{ #category : 'tests' }
ASOptimizingVariableOwnershipTest >> testBlockVariable [
	"If a variable is only referenced in the method, it will be put in the Context (or just on the stack if no context is created)."

	| method location1 location2 flow element1 element2 |
	method := cg basicBlocks first.
	flow := ASFlowNew new basicBlock: method.

	location1 := AStackLocation new.
	stack := ASStackTemporary fromStack: stack.
	element1 := stack location: location1.
	location1 stackValue: (AStackValue fromValue: 1).
	"method addOperation:( ASCPushVariable new with: (ASLocalNamed new
			             name: #l1;
			             location: location1))."
	method addOperation: (ASCPushVariable new with: element1).

	location2 := AStackLocation new.
	stack := ASStackTemporary fromStack: stack.
	element2 := stack location: location2.
	location2 stackValue: (AStackValue fromValue: 2).
	"method addOperation: (ASCPopVariable new with: (ASLocalNamed new
			            name: #l2;
			            location: location2))."
	method addOperation: (ASCPopVariable new with: element2).

	cg deriveOwnership.

	self assert: (method owns includes: element1).
	self assert: (method owns includes: element2)
]

{ #category : 'tests' }
ASOptimizingVariableOwnershipTest >> testMultipleBlockReferenceOneBlockModifies [
	" For values referenced in two or more places, modified in at least one, the default would be to put them in the Context. However, if a BlockClosure has a reference to the Context and the closure gets moved to the heap, it will drag the entire stack with it. Therefore the only closures that reference the context will be ones with non-local returns (or that create closures that need a context reference). Variables referenced in non-local-return closures will be placed in the context."

	| method bb1 location1 location2 flow |
	method := cg basicBlocks first.
	method label: #method.
	flow := ASFlowNew new basicBlock: method.

	location1 := AStackLocation new.
	location1 stackValue: (AStackValue fromValue: 1).

	location2 := AStackLocation new.
	location2 stackValue: (AStackValue fromValue: 2).

	method addOperation: (ASCPushVariable new with: location1).
	method addOperation: (ASCPopVariable new with: location2).

	bb1 := ASCCompiledClosureBlock new label: 'bb1'.
	bb1 addOperation: (ASCPushVariable new with: location1).
	bb1 addOperation: (ASCPopVariable new with: location1).

	cg basicBlocks add: bb1.


	cg deriveOwnership.

	self assert: (method owns includes: location1) not.
	self assert: (method owns includes: location2).
	self assert: (bb1 owns includes: location1)
]

{ #category : 'tests' }
ASOptimizingVariableOwnershipTest >> testMultipleBlockReferenceOneBlockModifiesWithNonLocalReturn [
	" For values referenced in two or more places, modified in at least one, the default would be to put them in the Context. However, if a BlockClosure has a reference to the Context and the closure gets moved to the heap, it will drag the entire stack with it. Therefore the only closures that reference the context will be ones with non-local returns (or that create closures that need a context reference). Variables referenced in non-local-return closures will be placed in the context."

	| method bb1 location1 location2 flow location3 |
	method := cg basicBlocks first.
	method label: #method.
	flow := ASFlowNew new basicBlock: method.

	location1 := AStackLocation new.
	location1 stackValue: (AStackValue fromValue: 1).

	location2 := AStackLocation new.
	location2 stackValue: (AStackValue fromValue: 2).

	location3 := AStackLocation new.
	location3 stackValue: (AStackValue fromValue: 3).
	
	method addOperation: (ASCPushVariable new with: location1).
	method addOperation: (ASCPopVariable new with: location2).

	bb1 := ASCCompiledClosureBlock new label: 'bb1'.
	bb1 addOperation: (ASCPushVariable new with: location1).
	bb1 addOperation: (ASCPopVariable new with: location1).
	bb1 addOperation: (ASCPushVariable new with: location3).
	bb1 returnNonLocal.

	cg basicBlocks add: bb1.


	cg deriveOwnership.

	self assert: (method owns includes: location1).
	self assert: (method owns includes: location2).
	self assert: (bb1 owns includes: location1) not.
	self assert: (bb1 owns includes: location2) not.
]

{ #category : 'tests' }
ASOptimizingVariableOwnershipTest >> testSimpleOneBlockDeclares [
"If a variable is only referenced in the method, it will be put in the Context (or just on the stack if no context is created)."

| method |
method := ASCCompiledMethodBlock new.
method declares add: #myVar.

method deriveOwnership.

self assert: (method owns isEmpty)
]

{ #category : 'tests' }
ASOptimizingVariableOwnershipTest >> testSimpleOneBlockDeclaresAndModifies [
"If a variable is only referenced in the method, it will be put in the Context (or just on the stack if no context is created)."

| method |
method := ASCCompiledMethodBlock new.
method declares add: #myVar.
method modified add: #myVar.

method deriveOwnership.

self assert: (method owns includes: #myVar)
]

{ #category : 'tests' }
ASOptimizingVariableOwnershipTest >> testSimpleOneBlockDeclaresAndReferences [
"If a variable is only referenced in the method, it will be put in the Context (or just on the stack if no context is created)."

| method |
method := ASCCompiledMethodBlock new.
method declares add: #myVar.
method referenced add: #myVar.

method deriveOwnership.

self assert: (method owns includes: #myVar) 
]

{ #category : 'tests' }
ASOptimizingVariableOwnershipTest >> testTwoBlocksOneBlockDeclaresOtherReferences [
"If a variable is only referenced in the method, it will be put in the Context (or just on the stack if no context is created)."

| method block |
method := ASCCompiledMethodBlock new.
method declares add: #myVar.

block := ASCCompiledClosureBlock new.
block referenced add: #myVar.

"how many passes?"
method deriveOwnership.
block deriveOwnership.

self assert: method owns isEmpty.
self assert: (block owns includes: #myVar).
]
