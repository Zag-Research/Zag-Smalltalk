Class {
	#name : 'ASZigThreadedOutputTest',
	#superclass : 'ASZigTest',
	#instVars : [
		'output'
	],
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
ASZigThreadedOutputTest >> setUp [

	super setUp.
	output := ASZigThreadedOutput testing context: file
]

{ #category : 'tests' }
ASZigThreadedOutputTest >> testZigAST_fib [

	| ast |
	ast := self astFor: ASCompileTestClass1 >> #fib:.
	self assert: ast equals: (ASMethodNode
			 selector: #fib:
			 arguments: #( anInt )
			 body: { { (ASReturn expression: (ASSend
						   target: (ASSend
								    target: (ASRef variable: #anInt)
								    selector: #<
								    args: { (ASLiteral literal: 2) })
						   selector: #ifTrue:ifFalse:
						   args: {
								   (ASBlock body: { { (ASLiteral literal: 1) } }).
								   (ASBlock body: { { (ASSend
										     target:
											     (ASSend
												      target: ASSelf
												      selector: #fib:
												      args: { (ASSend
														       target: (ASRef variable: #anInt)
														       selector: #-
														       args: { (ASLiteral literal: 1) }) })
										     selector: #+
										     args:
											     { (ASSend
												      target: ASSelf
												      selector: #fib:
												      args: { (ASSend
														       target: (ASRef variable: #anInt)
														       selector: #-
														       args: { (ASLiteral literal: 2) }) }) }) } }) })) } })
]

{ #category : 'tests' }
ASZigThreadedOutputTest >> testZig_addComment [

	| comment |
	comment := 'this is a comment!'.
	output asString;comment: comment.

	self assert: output asString equals: ' // ''' , comment , ''''
]

{ #category : 'tests' }
ASZigThreadedOutputTest >> testZig_addTwoNumbers [

	| ast |
	ast := self astFor: ASCompileTestClass1 >> #addTwoNumbers.
	output generateCode: ast.

	self assert: output name equals: 'MaddTwoNumbers'.
	self
		assert: output asString
		equals:
		'&e.verifySelector,&e.pushContext,"^",":MaddTwoNumbers:1",&e.pushLiteral,Object.from(4),&e.pushLiteral,Object.from(5),&e.send1,Sym.@"+",":MaddTwoNumbers:2",&e.storeLocal,1,&e.popLocal,0,&e.pushLocal,1,&e.pushLocal,0,&e.send1tail,Sym.@"+",'
]

{ #category : 'tests' }
ASZigThreadedOutputTest >> testZig_fib [

	| ast |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #fib:.
	output generateCode: ast.
	self assert: output name equals: 'Mfib:'.
	self
		assert: output asString
		equals: '&p.p1,&e.pushLiteral,False,&e.returnTop,'
]

{ #category : 'tests' }
ASZigThreadedOutputTest >> testZig_fibonacci [

	| ast |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #fibonacci.
	output generateCode: ast for: SmallInteger.
	output codeGenerator basicBlocksLinearizeDo: #inspect.
	self assert: output name equals: 'Mfibonacci'.
	self
		assert: output asString
		equals: '&p.p1,&e.pushLiteral,False,&e.returnTop,'
]

{ #category : 'tests' }
ASZigThreadedOutputTest >> testZig_goTo [

	output labelRef: #hello.

	self assert: output asString equals: '"hello"'
]

{ #category : 'tests' }
ASZigThreadedOutputTest >> testZig_prim [

	| ast |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #prim.
	output generateCode: ast.
	self assert: output name equals: 'Mprim'.
	self
		assert: output asString
		equals: '&p.p1,":Mprim:1",&e.pushLiteral,False,&e.returnTop,'
]

{ #category : 'tests' }
ASZigThreadedOutputTest >> testZig_pushLabel [

	self assert: output nextLabel equals: 'L1'.

	self assert: output asString equals: '":L1",'
]

{ #category : 'tests' }
ASZigThreadedOutputTest >> testZig_pushLiteral [

	output visitPushLiteral: 10.

	self
		assert: output asString
		equals: '&e.pushLiteral,Object.from(10),'
]

{ #category : 'tests' }
ASZigThreadedOutputTest >> testZig_pushReturn [

	output visitReturnTop: nil.

	self assert: output asString equals: '&e.returnTop,'
]

{ #category : 'tests' }
ASZigThreadedOutputTest >> testZig_simple1 [

	| ast |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #simple1.
	output generateCode: ast.
	self assert: output name equals: 'Msimple1'.
	self
		assert: output asString
		equals:
		'":Msimple1:1",&e.pushLiteral,Object.from(42),&e.returnTop,'
]

{ #category : 'tests' }
ASZigThreadedOutputTest >> testZig_simple1_noreturn [

	| ast |
	ast := self astFor:
		       ASCompileTestClass1 >> #simple1_noreturn.
	output generateCode: ast.
	self assert: output name equals: 'Msimple1_noreturn'.
	self
		assert: output asString
		equals:
		'":Msimple1_noreturn:1",&e.pushLiteral,Object.from(42),&e.drop,&e.returnSelf,'
]

{ #category : 'tests' }
ASZigThreadedOutputTest >> testZig_simple2 [

	| ast |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #simple2.
	output generateCode: ast.
	self assert: output name equals: 'Msimple2'.
	self
		assert: output asString
		equals:
		'&e.pushContext,"^",":Msimple2:1",&e.pushLiteral,Object.from(42),&e.returnTop,'
]

{ #category : 'tests' }
ASZigThreadedOutputTest >> testZig_simple2_noreturn [

	| ast |
	ast := self astFor:
		       ASCompileTestClass1 >> #simple2_noreturn.
	output generateCode: ast.
	self assert: output name equals: 'Msimple2_noreturn'.
	self
		assert: output asString
		equals:
		'&e.pushContext,"^",":Msimple2_noreturn:1",&e.pushLiteral,Object.from(42),&e.returnTop,'
]

{ #category : 'tests' }
ASZigThreadedOutputTest >> testZig_simpleBlock [

	| ast |
	ast := self astFor: ASCompileTestClass1 >> #simpleBlock.
	output generateCode: ast for: ASCompileTestClass1.
	self assert: output name equals: 'MsimpleBlock'.
	self
		assert: output asString
		equals:
		'&e.pushContext,"^",":MsimpleBlock:1",doSomething,&e.pushLocal,0,&e.pushLiteral,Object.from(42),&e.send1,Sym.@"value:",":MsimpleBlock:2",&e.returnTop,'
]

{ #category : 'tests' }
ASZigThreadedOutputTest >> testZig_unaries [

	| ast |
	ast := self astFor: ASCompileTestClass1 >> #unaries.
	output generateCode: ast for: ASCompileTestClass1.
	self assert: output name equals: 'Munaries'.
	self
		assert: output asString
		equals:
		'&e.pushContext,"^",":Munaries:1",&e.pushLiteral,Object.from(23),&e.send0,Sym.negated,":Munaries:2",&e.send0tail,Sym.i_1,'
]

{ #category : 'tests' }
ASZigThreadedOutputTest >> testZig_unariesPlus [

	| ast |
	ast := self astFor:
		       ASCompileTestClass1 >> #unariesPlus.
	output generateCode: ast for: ASCompileTestClass1.
	self assert: output name equals: 'MunariesPlus'.
	self
		assert: output asString
		equals:
		'&e.verifySelector,&e.pushContext,"^",":MunariesPlus:1",&e.pushLiteral,Object.from(42),&e.send0,Sym.negated,":MunariesPlus:2",&e.drop,&e.pushLiteral,Object.from(23),&e.send0,Sym.negated,":MunariesPlus:3",&e.send0,Sym.i_1,":MunariesPlus:4",&e.pushLiteral,Object.from(42),&e.send1tail,Sym.i_2,'
]

{ #category : 'tests' }
ASZigThreadedOutputTest >> testZig_unaries_notail [

	| ast |
	ast := self astFor: ASCompileTestClass1 >> #unaries.
	output
		tailCallOptimize: false;
		generateCode: ast for: ASCompileTestClass1.
	self assert: output name equals: 'Munaries'.
	self
		assert: output asString
		equals:
		'&e.verifySelector,&e.pushContext,"^",":Munaries:1",&e.pushLiteral,Object.from(23),&e.send0,Sym.negated,":Munaries:2",&e.send0,Sym.i_1,&e.returnTop.'
]
