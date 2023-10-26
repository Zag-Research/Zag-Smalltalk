Class {
	#name : #ASZigInspectionTest,
	#superclass : #TestCase,
	#category : #'ASTSmalltalk-Tests-Tests'
}

{ #category : #tests }
ASZigInspectionTest >> testInspectionMadeUpAST [

	| block |
	block := ASCMethodBlock new.
	block locals: { (#size , #location) } arguments: { #aSample }.
	block inspect
]

{ #category : #tests }
ASZigInspectionTest >> testInspectionOfFib [

	| ast file output |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #fib:.

	file := ASZigTestContext new.
	output := ASZigCPSOutput testing
		          context: file;
		          generateCode: ast.
		
	output codeGenerator firstBlock inspect
]
