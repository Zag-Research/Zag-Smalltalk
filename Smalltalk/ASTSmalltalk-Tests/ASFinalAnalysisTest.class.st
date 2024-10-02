Class {
	#name : 'ASFinalAnalysisTest',
	#superclass : 'TestCase',
	#instVars : [
		'simpleBlock'
	],
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
ASFinalAnalysisTest >> setUp [
	super setUp.

	simpleBlock := ASCBasicBlock new pushLiteral: 42;pushLiteral: 17
]
