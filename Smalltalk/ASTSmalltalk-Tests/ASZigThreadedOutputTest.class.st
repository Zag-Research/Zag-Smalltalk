Class {
	#name : #ASZigThreadedOutputTest,
	#superclass : #TestCase,
	#category : #'ASTSmalltalk-Tests-Tests'
}

{ #category : #tests }
ASZigThreadedOutputTest >> testZig_addComment [

	| output comment |
	comment := 'this is a comment!'.
	output := ASZigThreadedOutput new.
	output comment: comment.

	self assert: output asString equals: ' // ''' , comment , ''''
]

{ #category : #tests }
ASZigThreadedOutputTest >> testZig_addTwoNumbers [

	| output ast file |
	ast := ASPharoCompiler new astFor:
		       ASCompileTestClass1 >> #addTwoNumbers.
	file := ASZigTestContext new.
	output := ASZigThreadedOutput testing
		          context: file;
		          generateCode: ast.

	self assert: output name equals: 'MaddTwoNumbers'.
	self
		assert: output asString
		equals:
		'&e.verifySelector,&e.pushContext,"^",":MaddTwoNumbers:1",&e.pushLiteral,Object.from(4),&e.pushLiteral,Object.from(5),&e.send1,Sym.@"+",":MaddTwoNumbers:2",&e.storeLocal,1,&e.popLocal,0,&e.pushLocal,1,&e.pushLocal,0,&e.send1,Sym.@"+",":MaddTwoNumbers:3",&e.returnTop,' 
]

{ #category : #tests }
ASZigThreadedOutputTest >> testZig_fib [

	| output ast file |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #fib:.
	file := ASZigFileContext new.
	output := ASZigThreadedOutput testing
		          context: file;
		          generateCode: ast.
	self assert: output name equals: 'Mfib:'.
	self
		assert: output asString
		equals: '&p.p1,&e.pushLiteral,False,&e.returnTop,'
]

{ #category : #tests }
ASZigThreadedOutputTest >> testZig_fibonacci [

	| output ast file |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #fibonacci.
	file := ASZigFileContext new.
	output := ASZigThreadedOutput testing
		          context: file;
		          generateCode: ast for: SmallInteger.
	self assert: output name equals: 'Mfibonacci'.
	self
		assert: output asString
		equals: '&p.p1,&e.pushLiteral,False,&e.returnTop,'
]

{ #category : #tests }
ASZigThreadedOutputTest >> testZig_goTo [

	| output |
	output := ASZigThreadedOutput new.
	output labelRef: #hello.

	self assert: output asString equals: '"hello"'
]

{ #category : #tests }
ASZigThreadedOutputTest >> testZig_prim [

	| output ast file |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #prim.
	file := ASZigFileContext new.
	output := ASZigThreadedOutput testing
		             context: file;
		             generateCode: ast.
	self assert: output name equals: 'Mprim'.
	self
		assert: output asString
		equals: '&p.p1,":Mprim:1",&e.pushLiteral,False,&e.returnTop,'
]

{ #category : #tests }
ASZigThreadedOutputTest >> testZig_pushDrop [

	| output |
	output := ASZigThreadedOutput  testing.
	output visitDrop.

	self assert: output asString equals: '&e.drop,'
]

{ #category : #tests }
ASZigThreadedOutputTest >> testZig_pushLabel [

	| output |
	output := ASZigThreadedOutput new.
	self assert: output nextLabel equals: 'L1'.

	self assert: output asString equals: '":L1",'
]

{ #category : #tests }
ASZigThreadedOutputTest >> testZig_pushLiteral [

	| output |
	output := ASZigThreadedOutput  testing.
	output visitPushLiteral: 10.

	self
		assert: output asString
		equals: '&e.pushLiteral,Object.from(10),'
]

{ #category : #tests }
ASZigThreadedOutputTest >> testZig_pushReturn [

	| output |
	output := ASZigThreadedOutput testing.
	output visitReturnTop.

	self assert: output asString equals: '&e.returnTop,'
]

{ #category : #tests }
ASZigThreadedOutputTest >> testZig_simple1 [

	| output ast file |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #simple1.
	file := ASZigFileContext new.
	output := ASZigThreadedOutput testing
		             context: file;
		             generateCode: ast.
	self assert: output name equals: 'Msimple1'.
	self
		assert: output asString
		equals:
		'&e.verifySelector,":Msimple1:1",&e.pushLiteral,Object.from(42),&e.returnTop,'
]

{ #category : #tests }
ASZigThreadedOutputTest >> testZig_simpleBlock [

	| output ast file |
	ast := ASPharoCompiler new astFor:
		       ASCompileTestClass1 >> #simpleBlock.
	file := ASZigFileContext new.
	output := ASZigThreadedOutput testing
		          context: file;
		          generateCode: ast for: ASCompileTestClass1.
	self assert: output name equals: 'MsimpleBlock'.
	self
		assert: output asString
		equals:
		'&e.verifySelector,&e.pushContext,"^",":MsimpleBlock:1",doSomething,&e.pushLiteral,Object.from(42),&e.send1,Sym.@"value:",&e.returnTop,'
]

{ #category : #tests }
ASZigThreadedOutputTest >> testZig_unaries [

	| output ast file |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #unaries.
	file := ASZigFileContext new.
	output := ASZigThreadedOutput testing
		          context: file;
		          generateCode: ast for: ASCompileTestClass1.
	self assert: output name equals: 'Munaries'.
	self
		assert: output asString
		equals:
		'&e.verifySelector,&e.pushContext,"^",":Munaries:1",&e.pushLiteral,Object.from(23),&e.send0,Sym.negated,":Munaries:2",&e.send0,Sym.i_1,":Munaries:3",&e.returnTop,'
]

{ #category : #tests }
ASZigThreadedOutputTest >> testZig_unariesPlus [

	| output ast file |
	ast := ASPharoCompiler new astFor:
		       ASCompileTestClass1 >> #unariesPlus.
	file := ASZigFileContext new.
	output := ASZigThreadedOutput testing
		          context: file;
		          generateCode: ast for: ASCompileTestClass1.
	self assert: output name equals: 'MunariesPlus'.
	self
		assert: output asString
		equals:
		'&e.verifySelector,&e.pushContext,"^",":MunariesPlus:1",&e.pushLiteral,Object.from(23),&e.send0,Sym.negated,":MunariesPlus:2",&e.send0,Sym.i_1,":MunariesPlus:3",&e.pushLiteral,Object.from(42),&e.send1,Sym.i_2,":MunariesPlus:4",&e.returnTop,'
]
