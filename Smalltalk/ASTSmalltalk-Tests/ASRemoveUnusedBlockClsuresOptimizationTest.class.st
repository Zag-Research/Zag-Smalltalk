Class {
	#name : 'ASRemoveUnusedBlockClsuresOptimizationTest',
	#superclass : 'ASFinalAnalysisTest',
	#instVars : [
		'cg'
	],
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
ASRemoveUnusedBlockClsuresOptimizationTest >> setUp [

	super setUp.

	"Put here a common initialization logic for tests"
	ASInliner reset.
	cg := ASCodeGenerator
		      targetClass: ASCompileTestClass1
		      selector: #blockReturnEasy.
	cg removeUnusedBlockClosures.
	cg inspect
]

{ #category : 'tests' }
ASRemoveUnusedBlockClsuresOptimizationTest >> test [

	| compileMethod |
	compileMethod := cg basicBlocks first.

	self assert: (compileMethod operationAt: 2) isASCNil.
	self assert: (compileMethod operationAt: 3) isASCNil
]
