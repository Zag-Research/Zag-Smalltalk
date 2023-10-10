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
	self
		assert: output asString
		equals:
		'&e.verifySelector,":MsimpleBlock:1",doSomething,&e.pushLiteral,Object.from(42),&e.send1,Sym.@"value:",&e.returnTop,'
]

{ #category : #tests }
ASZigCPSOutputTest >> testZig_unaries [

	| output ast file |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #unaries.
	file := ASZigFileContext new.
	output := ASZigCPSOutput testing
		          context: file;
		          generateCodeFor: ast.
	self assert: output name equals: 'Munaries'.
	self
		assert: output asString
		equals:
		'&e.verifySelector,":Munaries:1",&e.pushLiteral,Object.from(23),&e.send0,Sym.negated,&e.send0,Sym.i_1,&e.returnTop,'
]

{ #category : #tests }
ASZigCPSOutputTest >> testZig_unaries_break [

	| output ast file |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #unaries.
	file := ASZigFileContext new.
	output := ASZigCPSOutput testing breakAtCall
		          context: file;
		          generateCodeFor: ast.
	self assert: output name equals: 'Munaries'.
	self
		assert: output asString
		equals:
		'&e.verifySelector,":Munaries:1",&e.pushLiteral,Object.from(23),&e.send0,Sym.negated,":Munaries:2",&e.send0,Sym.i_1,":Munaries:3",&e.returnTop,'
]
