Class {
	#name : 'ASInlineWherePossibleTest',
	#superclass : 'ASFinalAnalysisTest',
	#instVars : [
		'cgNoInline',
		'fooBarBlockMethodAST',
		'cgInline'
	],
	#category : 'Zag-Core-Tests-Tests',
	#package : 'Zag-Core-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
ASInlineWherePossibleTest >> setUp [

	super setUp.
	"Put here a common initialization logic for tests"
	ASInliner reset.
	cgNoInline := (ASCodeGenerator
		               targetClass: ASCompileTestClass1
		               selector: #fooBarBlock)
		              inline: 0;
		              finalAnalysis.
	ASInliner reset.
	cgInline := (ASCodeGenerator
		               targetClass: ASCompileTestClass1
		               selector: #fooBarBlock)
		              inline: 1000;
		              finalAnalysis.
	cgInline inspect.
	
	fooBarBlockMethodAST := ASCompileTestClass1 >> #fooBarBlock
]

{ #category : 'tests' }
ASInlineWherePossibleTest >> testFindMethodBlock [

	self
		assert: (cgNoInline basicBlocks size)
		equals: 1.
		
	self
		assert: (cgInline basicBlocks size)
		equals: 5
]
