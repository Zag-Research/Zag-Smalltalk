Class {
	#name : 'ASRemoveUnusedBlockClsuresOptimizationTest',
	#superclass : 'ASFinalAnalysisTest',
	#instVars : [
		'cgNoInline',
		'cgInline'
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
	cgNoInline := (ASCodeGenerator
		      targetClass: ASCompileTestClass1
		      selector: #blockReturnEasy)
				inline: false;
				removeUnusedBlockClosures.
	
	ASInliner reset.
	cgInline := (ASCodeGenerator
		      targetClass: ASCompileTestClass1
		      selector: #blockReturnEasy)
				inline: true; 
				removeUnusedBlockClosures.
]

{ #category : 'tests' }
ASRemoveUnusedBlockClsuresOptimizationTest >> test [

	| compileMethod |
	compileMethod := cgInline basicBlocks first.

	self assert: (compileMethod operationAt: 2) isASCNil.
	self assert: (compileMethod operationAt: 3) isASCNil.
	
	compileMethod := cgNoInline basicBlocks first.

	self assert: (compileMethod operationAt: 2) isBlock.
	self assert: (compileMethod operationAt: 3) isBlock
]
