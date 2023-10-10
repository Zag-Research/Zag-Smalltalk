Class {
	#name : #ASZigCPSOutputTest,
	#superclass : #TestCase,
	#category : #'ASTSmalltalk-Tests-Tests'
}

{ #category : #tests }
ASZigCPSOutputTest >> testZig_addTwoNumbers [

	| output ast file |
	ast := ASPharoCompiler new astFor:
		       ASCompileTestClass1 >> #addTwoNumbers.
	file := ASZigFileContext new.
	output := ASZigCPSOutput testing
		          context: file;
		          generateCodeFor: ast.

	self assert: output name equals: 'MaddTwoNumbers'.
	output asString inspect.

]

{ #category : #tests }
ASZigCPSOutputTest >> testZig_fib [

	| output ast file |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #fib:.
	file := ASZigFileContext new.
	output := ASZigCPSOutput testing
		             context: file;
		             generateCodeFor: ast.
	self assert: output name equals: 'Mfib'.
	self
		assert: output asString
		equals: '&p.p1,&e.pushLiteral,False,&e.returnTop,'
]

{ #category : #tests }
ASZigCPSOutputTest >> testZig_simple1 [

	| output ast file |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #simple1.
	file := ASZigFileContext new.
	output := ASZigCPSOutput testing
		          context: file;
		          generateCodeFor: ast.
	self assert: output name equals: 'Msimple1'.
	self
		assert: output asString
		equals:
		'&e.verifySelector,":Msimple1:1",&e.pushLiteral,Object.from(42),&e.returnTop,'
]

{ #category : #tests }
ASZigCPSOutputTest >> testZig_simpleBlock [

	| output ast file |
	ast := ASPharoCompiler new astFor:
		       ASCompileTestClass1 >> #simpleBlock.
	file := ASZigFileContext new.
	output := ASZigCPSOutput testing
		          context: file;
		          generateCodeFor: ast.
	self assert: output name equals: 'MsimpleBlock'.
	file asString inspect.
	self
		assert: output asString
		equals:
		'&e.verifySelector,":MsimpleBlock:1",doSomething,&e.pushLiteral,Object.from(42),&e.send1,Sym.@"value:",&e.returnTop,'
]

{ #category : #tests }
ASZigCPSOutputTest >> testZig_unaries [

	| output ast file |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #unaries.
	file := ASZigTestContext new.
	output := ASZigCPSOutput testing
		          context: file;
		          generateCodeFor: ast.
	self assert: output name equals: 'Munaries'.
	file asString inspect

]

{ #category : #tests }
ASZigCPSOutputTest >> testZig_unariesPlus [

	| output ast file |
	ast := ASPharoCompiler new astFor:
		       ASCompileTestClass1 >> #unariesPlus.
	file := ASZigTestContext new.
	output := ASZigCPSOutput testing
		          context: file;
		          generateCodeFor: ast.
	self assert: output name equals: 'MunariesPlus'.
	file asString inspect
]
