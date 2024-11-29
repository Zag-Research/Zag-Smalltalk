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
AStackFlowTest >> testBar [

	| cg stackElem |
	cg := ASCodeGenerator targetClass: ASCompileTestClass1 selector: #bar.
	cg finalAnalysis.
	cg flowAnalysis.
	stackElem := cg basicBlocks first stack at: 0.
	
	"Top stack element is a temp, hence is annotated with `new`"
	self assert: (stackElem location flowValue class) equals: ASFlowNew.
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

	"Merge `ASCBasicBlock`s together"
	| joinBlock |
	joinBlock := ASCJoinBlock new.
	joinBlock branchedToFrom: simpleBlock1.
	joinBlock branchedToFrom: simpleBlock2.
	joinBlock addFlow.
	
	self assert: (joinBlock stack flow) class equals: ASFlowPhi.
	self assert: (joinBlock stack flow) asAssociationList asSet equals: {
			(simpleBlock1 -> flowNew1).
			(simpleBlock2 -> flowNew2) } asSet.
]

{ #category : 'tests' }
AStackFlowTest >> testPlusAndAnd [

	| cg result params allAnnotatedOnDemand |
	ASInliner reset.
	cg := ASCodeGenerator halt;
		      targetClass: ASCompileTestClass1
		      selector: #plus:and:and:.
	cg finalAnalysis.
	cg flowAnalysis.
	cg inspect.

	result := cg basicBlocks second stack at: 0.
	params := #( n1 n2 n3 ) collect: [ :paramName |
		          cg basicBlocks first stack at: paramName ].
	allAnnotatedOnDemand := params allSatisfy: [ :param |
		                        param location flowValue class
		                        = ASFlowOnDemand ].

	"All parameters are annotated with `on Demand`"
	self assert: allAnnotatedOnDemand.
	"Top stack element is the temp result, hence is annotated with `new`"
	self assert: result location flowValue class equals: ASFlowNew
]
