"
I test algorithm that optimizes the ownership of variables in blocks. See https://github.com/Zag-Research/Zag-Smalltalk/edit/main/Documentation/Final%20Analysis.md#optimizing-local-variable-locations for more information.
"
Class {
	#name : 'ASOptimizingVariableOwnershipTest',
	#superclass : 'TestCase',
	#instVars : [
		'cg',
		'stack',
		'method',
		'flow',
		'location1',
		'element1',
		'location2',
		'element2',
		'location3',
		'element3',
		'bb1',
		'bb2'
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
	stack := first stack.

	method := cg basicBlocks first.
	method label: #method.
	flow := ASFlowNew new basicBlock: method.

	location1 := AStackLocation new.
	stack := ASStackTemporary fromStack: stack.
	element1 := stack location: location1.
	location1 stackValue: (AStackValue fromValue: 1).

	location2 := AStackLocation new.
	stack := ASStackTemporary fromStack: stack.
	element2 := stack location: location2.
	location2 stackValue: (AStackValue fromValue: 2).

	location3 := AStackLocation new.
	stack := ASStackTemporary fromStack: stack.
	element3 := stack location: location3.
	location3 stackValue: (AStackValue fromValue: 3).

	bb1 := ASCCompiledClosureBlock new label: 'bb1'.
	cg compiledUnits add: bb1.
	
	bb2 := ASCCompiledClosureBlock new label: 'bb2'.
	cg compiledUnits add: bb2
]

{ #category : 'tests' }
ASOptimizingVariableOwnershipTest >> testBlockVariable [
	"If a variable is only referenced in the method, it will be put in the Context (or just on the stack if no context is created)."
	
	method addOperation: (ASCPushVariable new with: element1).
	method addOperation: (ASCPopVariable new with: element2).

	cg optimizeLocalVariableLocations.

	self assert: (method owns includes: element1).
	self assert: (method owns includes: element2)
]

{ #category : 'tests' }
ASOptimizingVariableOwnershipTest >> testMultipleBlockModifieds [
	" For values referenced in two or more places, modified in at least one, the default would be to put them in the Context. However, if a BlockClosure has a reference to the Context and the closure gets moved to the heap, it will drag the entire stack with it. Therefore the only closures that reference the context will be ones with non-local returns (or that create closures that need a context reference). Variables referenced in non-local-return closures will be placed in the context."

	bb1 addOperation: (ASCPushVariable new with: element1).
	bb1 addOperation: (ASCPopVariable new with: element1).

	bb2 addOperation: (ASCPushVariable new with: element1).
	bb2 addOperation: (ASCPopVariable new with: element1).

	cg optimizeLocalVariableLocations.

	self
		assert: ((bb1 owns includes: element1)
		xor: [ bb2 owns includes: element1 ])
]

{ #category : 'tests' }
ASOptimizingVariableOwnershipTest >> testMultipleBlockReference [
	" For values referenced in two or more places, modified in at least one, the default would be to put them in the Context. However, if a BlockClosure has a reference to the Context and the closure gets moved to the heap, it will drag the entire stack with it. Therefore the only closures that reference the context will be ones with non-local returns (or that create closures that need a context reference). Variables referenced in non-local-return closures will be placed in the context."

	bb1 addOperation: (ASCPushVariable new with: element1).
	
	bb2 addOperation: (ASCPushVariable new with: element1).
	
	cg optimizeLocalVariableLocations.

	self assert: (bb1 owns includes: element1) not.
	self assert: (bb2 owns includes: element1) not.
	self assert: (method owns includes: element1).
]

{ #category : 'tests' }
ASOptimizingVariableOwnershipTest >> testMultipleBlockReferenceAndOneModifiesWithNonLocalReturn [
	" For values referenced in two or more places, modified in at least one, the default would be to put them in the Context. However, if a BlockClosure has a reference to the Context and the closure gets moved to the heap, it will drag the entire stack with it. Therefore the only closures that reference the context will be ones with non-local returns (or that create closures that need a context reference). Variables referenced in non-local-return closures will be placed in the context."

	bb1 addOperation: (ASCPushVariable new with: element1).
	
	bb2 addOperation: (ASCPushVariable new with: element1).
	bb2 addOperation: (ASCPopVariable new with: element1).
	
	cg optimizeLocalVariableLocations.

	self assert: (bb1 owns includes: element1) not.
	self assert: (bb2 owns includes: element1).
]

{ #category : 'tests' }
ASOptimizingVariableOwnershipTest >> testMultipleBlockReferenceOneBlockModifies [
	" For values referenced in two or more places, modified in at least one, the default would be to put them in the Context. However, if a BlockClosure has a reference to the Context and the closure gets moved to the heap, it will drag the entire stack with it. Therefore the only closures that reference the context will be ones with non-local returns (or that create closures that need a context reference). Variables referenced in non-local-return closures will be placed in the context."

	method addOperation: (ASCPushVariable new with: element2).
	method addOperation: (ASCPopVariable new with: element2).

	bb1 addOperation: (ASCPushVariable new with: element1).
	bb1 addOperation: (ASCPopVariable new with: element1).

	cg optimizeLocalVariableLocations.

	self assert: (method owns includes: element1) not.
	self assert: (method owns includes: element2).
	self assert: (bb1 owns includes: element1)
]

{ #category : 'tests' }
ASOptimizingVariableOwnershipTest >> testMultipleBlockReferenceOneBlockModifiesWithNonLocalReturn [
	" For values referenced in two or more places, modified in at least one, the default would be to put them in the Context. However, if a BlockClosure has a reference to the Context and the closure gets moved to the heap, it will drag the entire stack with it. Therefore the only closures that reference the context will be ones with non-local returns (or that create closures that need a context reference). Variables referenced in non-local-return closures will be placed in the context."

	method addOperation: (ASCPushVariable new with: element1).
	method addOperation: (ASCPopVariable new with: element2).
	
	bb1 addOperation: (ASCPushVariable new with: element1).
	bb1 addOperation: (ASCPopVariable new with: element1).
	bb1 addOperation: (ASCPushVariable new with: element3).
	bb1 returnNonLocal.

	cg optimizeLocalVariableLocations.

	self assert: (method owns includes: element1).
	self assert: (method owns includes: element2).
	self assert: (bb1 owns includes: element1) not.
	self assert: (bb1 owns includes: element2) not
]
