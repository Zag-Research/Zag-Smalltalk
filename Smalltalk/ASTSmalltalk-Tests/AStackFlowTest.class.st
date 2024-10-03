Class {
	#name : 'AStackFlowTest',
	#superclass : 'ASFinalAnalysisTest',
	#instVars : [
		'var1',
		'simpleBlock1',
		'simpleBlock2',
		'f1',
		'f2',
		'stackTemp1',
		'flowNew1',
		'flowNew2'
	],
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
AStackFlowTest >> setUp [
	super setUp.

	simpleBlock1 := ASCBasicBlock new pushLiteral: 23; addFlow.
	simpleBlock2 := ASCBasicBlock new pushLiteral: 43; addFlow.
	"simpleBlockVar := ASCBasicBlock new pushVariable:"
	flowNew1 := simpleBlock1 stack flow.
	flowNew2 := simpleBlock2 stack flow.
	
"push additional vals, test3"
]

{ #category : 'tests' }
AStackFlowTest >> testPhi1 [

	"Merge `ASFlowNew`s together"
	self assert: (flowNew1 merge: flowNew2) class equals: ASFlowPhi.
	self assert: (flowNew1 merge: flowNew2) asAssociationList asSet equals: {
			(simpleBlock1 -> flowNew1).
			(simpleBlock2 -> flowNew2) } asSet
]

{ #category : 'tests' }
AStackFlowTest >> testPhi2 [

	"Merge `ASCBasicBlock`s together - in-progress"
	| basicBlock |
	basicBlock := ASCBasicBlock new.
	basicBlock branchedToFrom: simpleBlock1.
	basicBlock branchedToFrom: simpleBlock2.
	basicBlock merge.
	
	self assert: (basicBlock stack flow) class equals: ASFlowPhi.
	
	self assert: (basicBlock stack flow) asAssociationList asSet equals: {
			(simpleBlock1 -> flowNew1).
			(simpleBlock2 -> flowNew2) } asSet.
]
