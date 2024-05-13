Class {
	#name : 'ASZigInspectionTest',
	#superclass : 'ASZigTests',
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
ASZigInspectionTest >> testInspectionMadeUpAST [

	| block |
	block := ASCMethodBlock new.
	block locals: { (#size , #location) } arguments: { #aSample }.
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

	output codeGenerator firstBlock inspect
]
