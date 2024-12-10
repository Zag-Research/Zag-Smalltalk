"
I test algorithm that optimizes the ownership of variables in blocks. See https://github.com/Zag-Research/Zag-Smalltalk/edit/main/Documentation/Final%20Analysis.md#optimizing-local-variable-locations for more information.
"
Class {
	#name : 'ASOptimizingVariableOwnershipTest',
	#superclass : 'ASFinalAnalysisTest',
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
	#category : 'Zag-Core-Tests-Tests',
	#package : 'Zag-Core-Tests',
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
	"a method references element1 and modifies element1 so it owns both"

	method addOperation: (ASCPushVariable new with: element1).
	method addOperation: (ASCPopVariable new with: element2).

	cg optimizeLocalVariableLocations.

	self assert: (method owns includes: element1).
	self assert: (method owns includes: element2)
]

{ #category : 'tests' }
ASOptimizingVariableOwnershipTest >> testMultipleBlockModifieds [
	"both bb1 and bb2 refernece and modify element1 so either could own element1"

	bb1 addOperation: (ASCPushVariable new with: element1).
	bb1 addOperation: (ASCPopVariable new with: element1).

	bb2 addOperation: (ASCPushVariable new with: element1).
	bb2 addOperation: (ASCPopVariable new with: element1).

	cg optimizeLocalVariableLocations.

	self assert:
		((bb1 owns includes: element1) xor: [ bb2 owns includes: element1 ])
]

{ #category : 'tests' }
ASOptimizingVariableOwnershipTest >> testMultipleBlockReference [
	"  element1 is referenced by both bb1 and bb2 but since bb1 is a nonlocal return the method owns the element1"

	bb1 addOperation: (ASCPushVariable new with: element1).
	bb1 returnNonLocal.
	bb2 addOperation: (ASCPushVariable new with: element1).

	cg optimizeLocalVariableLocations.

	self assert: (bb1 owns includes: element1) not.
	self assert: (bb2 owns includes: element1) not.
	self assert: (method owns includes: element1)
	
]

{ #category : 'tests' }
ASOptimizingVariableOwnershipTest >> testMultipleBlockReferenceAndOneModifiesWithNonLocalReturn [
	" bb1 references element1 but bb2 owns it because it references and modifies element1"

	bb1 addOperation: (ASCPushVariable new with: element1).

	bb2 addOperation: (ASCPushVariable new with: element1).
	bb2 addOperation: (ASCPopVariable new with: element1).

	cg optimizeLocalVariableLocations.

	self assert: (bb1 owns includes: element1) not.
	self assert: (bb2 owns includes: element1)
]

{ #category : 'tests' }
ASOptimizingVariableOwnershipTest >> testMultipleBlockReferenceOneBlockModifies [
	"method referneces element2 and owns it, bb1 references and modified element2 and owns it"

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
	"method referneces element1 and writes element2
	bb1 references element1, writes element1 and references element3
	but! bb1 has a non local return so all vars owned by method"

	method addOperation: (ASCPushVariable new with: element1).
	method addOperation: (ASCPopVariable new with: element2).

	bb1 addOperation: (ASCPushVariable new with: element1).
	bb1 addOperation: (ASCPopVariable new with: element1).
	bb1 addOperation: (ASCPushVariable new with: element3).
	bb1 returnNonLocal.

	cg optimizeLocalVariableLocations.

	self assert: (method owns includes: element1).
	self assert: (method owns includes: element2).
	self assert: (method owns includes: element3).
	self assert: (bb1 owns includes: element1) not.
	self assert: (bb1 owns includes: element2) not.
	self assert: (bb1 owns includes: element3) not.
]
