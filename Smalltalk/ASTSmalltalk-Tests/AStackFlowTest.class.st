Class {
	#name : 'AStackFlowTest',
	#superclass : 'ASFinalAnalysisTest',
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
AStackFlowTest >> testPhi1 [

	| f1 f2 |
	f1 := ASFlowNew new
		      block: #one;
		      variable: 3.
	f2 := ASFlowNew new
		      block: #two;
		      variable: 4.
	self assert: (f1 merge: f2) class = ASFlowPhi.
	self assert: (f1 merge: f2) asAssociationList asSet equals: {
			(#one -> 3).
			(#two -> 4) } asSet
]
