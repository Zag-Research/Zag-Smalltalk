Class {
	#name : #ASZigCompileMethodTest,
	#superclass : #TestCase,
	#category : #'ASTSmalltalk-Tests-Tests'
}

{ #category : #tests }
ASZigCompileMethodTest >> testCompileMethod_simple1 [

	| ast codeGen file |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #simple1.
	file := ASZigFileContext new.
	codeGen := ASZigCompileMethodGenerator new testing context: file.
	codeGen generateCodeFor: ast.
	self assert: codeGen name equals: 'Msimple1'.
	self
		assert: codeGen asString
		equals:
		'&e.verifySelector,'
]
