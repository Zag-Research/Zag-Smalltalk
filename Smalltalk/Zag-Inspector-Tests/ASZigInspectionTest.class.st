Class {
	#name : 'ASZigInspectionTest',
	#superclass : 'ASZigTest',
	#category : 'Zag-Inspector-Tests-Tests',
	#package : 'Zag-Inspector-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
ASZigInspectionTest >> testInspectionMadeUpAST [

	| block |
	block := ASCCompiledMethodBlock new.
	block locals: { (#size , #location) } parameters: { #aSample }.
	block inspect
]

{ #category : 'tests' }
ASZigInspectionTest >> testInspectionOfZagASTForFib [

	(self astFor: ASCompileTestClass1 >> #fib:) inspect
]

{ #category : 'tests' }
ASZigInspectionTest >> testInspectionOfZagBlocksForFib [

	| ast output |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #fib:.

	output := ASZigCPSOutput testing
		          context: file;
		          generateCode: ast.

	output codeGenerator methodBlock inspect
]
