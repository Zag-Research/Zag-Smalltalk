Class {
	#name : 'ASFinalAnalysisTest',
	#superclass : 'TestCase',
	#instVars : [
		'simpleBlock'
	],
	#category : 'Zag-Core-Tests-Tests',
	#package : 'Zag-Core-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
ASFinalAnalysisTest >> setUp [
	super setUp.

	simpleBlock := ASCBasicBlock new pushLiteral: 42;pushLiteral: 17
]
