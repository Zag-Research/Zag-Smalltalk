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
	
	cgNoInline := (ASCodeGenerator
		               targetClass: ASCompileTestClass1
		               selector: #blockValueNonlocal)
		              inline: 0;
		              finalAnalysis.
	
	cgNoInline inspect.
	
	self assert: cgNoInline basicBlocks size equals: 2.
	
	ASInliner reset.
	cgInline := (ASCodeGenerator
		             targetClass: ASCompileTestClass1
		             selector: #blockValueNonlocal)
		            inline: 1000;
		            finalAnalysis.
	cgInline inspect.

	self assert: cgInline basicBlocks size equals: 3
]

{ #category : 'tests' }
ASInlineWherePossibleTest >> testEvaluateBlockValueWithBlock [
	"
blockValueWithBlock

	^ [ [ 4 ] value ] value
"

	| cgNoInline cgInline |
	"ASInliner reset.
	cgNoInline := (ASCodeGenerator
		               targetClass: ASCompileTestClass1
		               selector: #blockValueWithBlock)
		              inline: 0;
		              finalAnalysis.
	ASInliner reset.
	
	self
		assert: (cgNoInline basicBlocks size)
		equals: 1.
		
	"
	cgInline := (ASCodeGenerator
		             targetClass: ASCompileTestClass1
		             selector: #blockValueWithBlock)
		            inline: 1000;
		            finalAnalysis.
	cgInline inspect.


	self assert: cgInline basicBlocks size equals: 3
]

{ #category : 'tests' }
ASInlineWherePossibleTest >> testEvaluateBlockValueWithValue [
	"
blockValue

	^ [ 4 ] value 
"

	| cgNoInline cgInline |
	"ASInliner reset.
	cgNoInline := (ASCodeGenerator
		               targetClass: ASCompileTestClass1
		               selector: #bracketWithValue)
		              inline: 0;
		              finalAnalysis.
	ASInliner reset.
	
	self
		assert: (cgNoInline basicBlocks size)
		equals: 1.
		
	"
	cgInline := (ASCodeGenerator
		             targetClass: ASCompileTestClass1
		             selector: #bracketWithValue)
		            inline: 1000;
		            finalAnalysis.
	cgInline inspect.


	self assert: cgInline basicBlocks size equals: 3
]

{ #category : 'tests' }
ASInlineWherePossibleTest >> testEvaluatefib [
	"
fib: anInt



	^ anInt < 2
		  ifTrue: [ 1 ]
		  ifFalse: [ (self fib: anInt - 1) + (self fib: anInt - 2) ]
"
"see version in Integer"
	| cgNoInline cgInline |
	ASInliner reset.
	cgNoInline := (ASCodeGenerator
		               targetClass: ASCompileTestClass1
		               selector: #fib:)
		              inline: 0;
		              finalAnalysis.
	ASInliner reset.
	cgNoInline inspect.
	cgInline := (ASCodeGenerator
		             targetClass: ASCompileTestClass1
		             selector: #fib:)
		            inline: 1000;
		            finalAnalysis.
	cgInline inspect.

	self assert: cgNoInline basicBlocks size equals: 1.

	self assert: cgInline basicBlocks size equals: 3
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
"
fooBarBlock

	^ self bar: [ 42 ]
"
	| cgNoInline inlineBlock |
	ASInliner reset.
	cgNoInline := (ASCodeGenerator
		               targetClass: ASCompileTestClass1
		               selector: #fooBarBlock)
		              inline: 0;
		              finalAnalysis.

	inlineBlock := cgNoInline newInlineMethod:
		               ASCompileTestClass1 >> #fooBarBlock
]

{ #category : 'tests' }
ASInlineWherePossibleTest >> test_newInlineBlock_forClosureBlockAndNonLocalReturn [

	| methodBlock newInlineBlock blockAST |
	blockAST := ASBlockNode
		            arguments: #(  )
		            locals: #(  )
		            body: { (ASReturn expression: (ASLiteral literal: 4)) }.
	methodBlock := ASCInlineClosureBlock new.
	methodBlock locals: #() parameters: #().
	newInlineBlock := methodBlock newInlinedBlock: blockAST.

	self assert: newInlineBlock class equals: ASCTopMethodClosureBlock.
	self assert: newInlineBlock returnTop equals: ASCReturnTop.
]

{ #category : 'tests' }
ASInlineWherePossibleTest >> test_newInlineBlock_forClosureBlockAndReturnTop [

	| methodBlock newInlineBlock blockAST |
	blockAST := ASBlockNode
		            arguments: #(  )
		            locals: #(  )
		            body: { (ASLiteral literal: 4) }.
	methodBlock := ASCInlineClosureBlock new.
	newInlineBlock := methodBlock newInlinedBlock: blockAST.

	self assert: newInlineBlock class equals: ASCInlineClosureBlock.
	self assert: newInlineBlock returnTop equals: ASCReturnTop.
	self assert: newInlineBlock returnSelf equals: ASCReturnTop.
]

{ #category : 'tests' }
ASInlineWherePossibleTest >> test_newInlineBlock_forInlineMethodAndNonLocalReturn [

	| methodBlock newInlineBlock blockAST |
	blockAST := ASBlockNode
		            arguments: #(  )
		            locals: #(  )
		            body: { (ASReturn expression: (ASLiteral literal: 4)) }.
	methodBlock := ASCInlineMethodBlock new.
	newInlineBlock := methodBlock newInlinedBlock: blockAST.

	self assert: newInlineBlock class equals: ASCTopMethodClosureBlock.
	"self assert: newInlineBlock returnTop equals: ASCReturnTop."
]

{ #category : 'tests' }
ASInlineWherePossibleTest >> test_newInlineBlock_forInlineMethodAndReturnTop [

	| methodBlock newInlineBlock blockAST |
	blockAST := ASBlockNode
		            arguments: #(  )
		            locals: #(  )
		            body: { ASLiteral literal: 4 }.
	methodBlock := ASCInlineMethodBlock new.
	newInlineBlock := methodBlock newInlinedBlock: blockAST.

	self assert: newInlineBlock class equals: ASCInlineClosureBlock.
	"self assert: newInlineBlock returnTop equals: ASCReturnTop."
]

{ #category : 'tests' }
ASInlineWherePossibleTest >> test_newInlineBlock_forMethodAndNonLocalReturn [

	| methodBlock newInlineBlock blockAST |
	blockAST := ASBlockNode
		            arguments: #(  )
		            locals: #(  )
		            body: { (ASReturn expression: (ASLiteral literal: 4)) }.
	methodBlock := ASCCompiledMethodBlock new.
	newInlineBlock := methodBlock newInlinedBlock: blockAST.

	self assert: newInlineBlock class equals: ASCTopClosureBlock.
	"self assert: newInlineBlock returnTop equals: ASCReturnTop."

]

{ #category : 'tests' }
ASInlineWherePossibleTest >> test_newInlineBlock_forMethodAndReturnTop [

	| methodBlock newInlineBlock blockAST |
	blockAST := ASBlockNode
		            arguments: #(  )
		            locals: #(  )
		            body: { (ASLiteral literal: 4) }.
	methodBlock := ASCCompiledMethodBlock new.
	newInlineBlock := methodBlock newInlinedBlock: blockAST.

	self assert: newInlineBlock class equals: ASCInlineClosureBlock.
	"self assert: newInlineBlock returnTop equals: ASCReturnTop."
	"create a new test that determines when a context should be created and when after inlining the context is no longer required"

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
	self assert: newInlineBlock returnTop equals: ASCReturnTop.
	"create a new test that determines when a context should be created and when after inlining the context is no longer required"

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
	methodBlock := ASCInlineClosureBlock new.
	newInlineBlock := methodBlock newInlinedBlock: blockAST.

	self assert: newInlineBlock class equals: ASCInlineClosureBlock.

	blockAST := ASBlockNode
		            arguments: #(  )
		            locals: #(  )
		            body: { (ASReturn expression: (ASLiteral literal: 4)) }.
	methodBlock := ASCInlineClosureBlock new.
	newInlineBlock := methodBlock newInlinedBlock: blockAST.

	self assert: newInlineBlock class equals: ASCTopClosureBlock.
	
	
	blockAST := ASBlockNode
		            arguments: #(  )
		            locals: #(  )
		            body: { ASLiteral literal: 4 }.
	methodBlock := ASCInlineMethodBlock new.
	newInlineBlock := methodBlock newInlinedBlock: blockAST.

	self assert: newInlineBlock class equals: ASCInlineClosureBlock.
	
	blockAST := ASBlockNode
		            arguments: #(  )
		            locals: #(  )
		            body: { (ASReturn expression: (ASLiteral literal: 4)) }.
	methodBlock := ASCInlineMethodBlock new.
	newInlineBlock := methodBlock newInlinedBlock: blockAST.

	self assert: newInlineBlock class equals: ASCTopMethodClosureBlock
]
