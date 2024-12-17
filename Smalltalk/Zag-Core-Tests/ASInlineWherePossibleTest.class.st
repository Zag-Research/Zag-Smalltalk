Class {
	#name : 'ASInlineWherePossibleTest',
	#superclass : 'ASFinalAnalysisTest',
	#instVars : [
		'cgNoInline',
		'fooBarBlockMethodAST'
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
		              inline: false;
		              finalAnalysis.
	fooBarBlockMethodAST := ASCompileTestClass1 >> #fooBarBlock
]

{ #category : 'tests' }
ASInlineWherePossibleTest >> testFindMethodBlock [

	| aBlock |
	self
		assert: (cgNoInline recursiveBasicBlock: fooBarBlockMethodAST)
		equals: aBlock
]
