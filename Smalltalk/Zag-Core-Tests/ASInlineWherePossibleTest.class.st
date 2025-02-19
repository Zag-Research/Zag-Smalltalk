Class {
	#name : 'ASInlineWherePossibleTest',
	#superclass : 'ASFinalAnalysisTest',
	#category : 'Zag-Core-Tests-Tests',
	#package : 'Zag-Core-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
ASInlineWherePossibleTest >> testEvaluateBlockValue [
	"
blockValue

	^ [ 4 ] value 
"

	| cgNoInline cgInline |
	"ASInliner reset.
	cgNoInline := (ASCodeGenerator
		               targetClass: ASCompileTestClass1
		               selector: #blockValue)
		              inline: 0;
		              finalAnalysis.
	ASInliner reset.
	
	self
		assert: (cgNoInline basicBlocks size)
		equals: 1.
		
	"
	cgInline := (ASCodeGenerator
		             targetClass: ASCompileTestClass1
		             selector: #blockValue)
		            inline: 1000;
		            finalAnalysis.
	cgInline inspect.


	self assert: cgInline basicBlocks size equals: 3
]

{ #category : 'tests' }
ASInlineWherePossibleTest >> testEvaluateBlockValueNonlocal [
	"
blockValueNonlocal

	[ ^ 4 ] value
"

	| cgNoInline cgInline |
	ASInliner reset.
	"
	cgNoInline := (ASCodeGenerator
		               targetClass: ASCompileTestClass1
		               selector: #blockValueNonlocal)
		              inline: 0;
		              finalAnalysis.
	ASInliner reset.

	self assert: cgNoInline basicBlocks size equals: 1.
"
	cgInline := (ASCodeGenerator
		             targetClass: ASCompileTestClass1
		             selector: #blockValueNonlocal)
		            inline: 1000;
		            finalAnalysis.
	cgInline inspect.



	self assert: cgInline basicBlocks size equals: 3
]

{ #category : 'tests' }
ASInlineWherePossibleTest >> testEvaluatefib [

"
blockValue

	^ [ 4 ] value 
"
| cgNoInline cgInline |
ASInliner reset.
	cgNoInline := (ASCodeGenerator
		               targetClass: ASCompileTestClass1
		               selector: #fib:)
		              inline: 0;
		              finalAnalysis.
	ASInliner reset.
	cgInline := (ASCodeGenerator
		               targetClass: ASCompileTestClass1
		               selector: #fib:)
		              inline: 1000;
		              finalAnalysis.
	cgInline inspect.
	
	self
		assert: (cgNoInline basicBlocks size)
		equals: 1.
		
	self
		assert: (cgInline basicBlocks size)
		equals: 3
]

{ #category : 'tests' }
ASInlineWherePossibleTest >> testFindMethodBlock [

"
fooBarBlock
	^ self bar: [ 42 ]
	
bar: aBlock
	^ aBlock value
"
| cgNoInline cgInline fooBarBlockMethodAST |
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
	
	fooBarBlockMethodAST := ASCompileTestClass1 >> #fooBarBlock.
	
	self
		assert: (cgNoInline basicBlocks size)
		equals: 1.
		
	self
		assert: (cgInline basicBlocks size)
		equals: 4
]

{ #category : 'tests' }
ASInlineWherePossibleTest >> testNewInlineMethod [

	| cgNoInline inlineBlock |
	ASInliner reset.
	cgNoInline := (ASCodeGenerator
		               targetClass: ASCompileTestClass1
		               selector: #fooBarBlock)
		              inline: 0;
		              finalAnalysis.
	
	inlineBlock := cgNoInline newInlineMethod: ASCompileTestClass1 >> #fooBarBlock.
	
]

{ #category : 'tests' }
ASInlineWherePossibleTest >> test_new_inline_block [

	| methodBlock newInlineBlock blockAST |
	blockAST := ASBlockNode
		            arguments: #(  )
		            locals: #(  )
		            body: { (ASLiteral literal: 4) }.
	methodBlock := ASCCompiledMethodBlock new.
	newInlineBlock := methodBlock newInlinedBlock: blockAST.

	self assert: newInlineBlock class equals: ASCInlineClosureBlock.

	blockAST := ASBlockNode
		            arguments: #(  )
		            locals: #(  )
		            body: { (ASReturn expression: (ASLiteral literal: 4)) }.
	methodBlock := ASCCompiledMethodBlock new.
	newInlineBlock := methodBlock newInlinedBlock: blockAST.

	self assert: newInlineBlock class equals: ASCTopClosureBlock.

	blockAST := ASBlockNode
		            arguments: #(  )
		            locals: #(  )
		            body: { (ASLiteral literal: 4) }.
	methodBlock := ASCInlinedBlock new.
	newInlineBlock := methodBlock newInlinedBlock: blockAST.

	self assert: newInlineBlock class equals: ASCInlinedBlock.

	blockAST := ASBlockNode
		            arguments: #(  )
		            locals: #(  )
		            body: { (ASReturn expression: (ASLiteral literal: 4)) }.
	methodBlock := ASCInlinedBlock new.
	newInlineBlock := methodBlock newInlinedBlock: blockAST.

	self assert: newInlineBlock class equals: ASCTopMethodClosureBlock
]
