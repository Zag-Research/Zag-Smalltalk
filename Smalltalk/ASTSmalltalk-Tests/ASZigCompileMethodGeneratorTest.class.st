Class {
	#name : #ASZigCompileMethodGeneratorTest,
	#superclass : #TestCase,
	#category : #'ASTSmalltalk-Tests-Tests'
}

{ #category : #tests }
ASZigCompileMethodGeneratorTest >> testZigFileContext_addTwoNumbers [

	| output ast file |
	ast := ASPharoCompiler new astFor:
		       ASCompileTestClass1 >> #addTwoNumbers.
	file := ASZigFileContext new.
	output := ASZigThreadedOutput testing
		             context: file;
		             generateCodeFor: ast.
	self assert: output name equals: 'MaddTwoNumbers'.
	self
		assert: output asString
		equals:
		'&e.verifySelector,&e.pushLiteral,Object.from(4),&e.pushLiteral,Object.from(5),&e.send1,Sym.@"+",&e.popLocal,1,&e.pushLocal,1,&e.returnTop,'
]

{ #category : #tests }
ASZigCompileMethodGeneratorTest >> testZigFileContext_fib [

	| generator ast file |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #fib:.
	file := ASZigFileContext new.
	generator := ASZigThreadedOutput testing
		             context: file;
		             generateCodeFor: ast.
	self assert: generator name equals: 'Mfib'.
	self
		assert: generator asString
		equals: '&p.p1,&e.pushLiteral,False,&e.returnTop,'
]

{ #category : #tests }
ASZigCompileMethodGeneratorTest >> testZigFileContext_prim [

	| generator ast file |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #prim.
	file := ASZigFileContext new.
	generator := ASZigThreadedOutput testing
		             context: file;
		             generateCodeFor: ast.
	self assert: generator name equals: 'Mprim'.
	self
		assert: generator asString
		equals: '&p.p1,&e.pushLiteral,False,&e.returnTop,' 
]

{ #category : #tests }
ASZigCompileMethodGeneratorTest >> testZigFileContext_simple1 [

	| generator ast file |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #simple1.
	file := ASZigFileContext new.
	generator := ASZigThreadedOutput testing
		             context: file;
		             generateCodeFor: ast.
	self assert: generator name equals: 'Msimple1'.
	self
		assert: generator asString
		equals:
		'&e.verifySelector,&e.pushLiteral,Object.from(42),&e.returnTop,' 
]

{ #category : #tests }
ASZigCompileMethodGeneratorTest >> testZigFileContext_simpleBlock [

	| generator ast file |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #simpleBlock.
	file := ASZigFileContext new.
	generator := ASZigThreadedOutput testing
		             context: file;
		             generateCodeFor: ast.
	self assert: generator name equals: 'MsimpleBlock'.
	self
		assert: generator asString
		equals: '&p.p1,&e.pushLiteral,False,&e.returnTop,'
]

{ #category : #tests }
ASZigCompileMethodGeneratorTest >> testZigFileGenerator_addComment [

	| generator comment |
	comment := 'this is a comment!'.
	generator := ASZigThreadedOutput new.
	generator comment: comment.

	self assert: generator asString equals: ' // ''' , comment , '''' 
]

{ #category : #tests }
ASZigCompileMethodGeneratorTest >> testZigFileGenerator_goTo [

	| generator |
	generator := ASZigThreadedOutput new.
	generator goTo: #hello.

	self assert: generator asString equals: '"hello"' 
]

{ #category : #tests }
ASZigCompileMethodGeneratorTest >> testZigFileGenerator_pushDrop [

	| generator |
	generator := ASZigThreadedOutput new testing.
	generator drop.

	self assert: generator asString equals: '&e.drop,' 
]

{ #category : #tests }
ASZigCompileMethodGeneratorTest >> testZigFileGenerator_pushLabel [

	| generator |
	generator := ASZigThreadedOutput new.
	self assert: generator nextLabel equals: 'L1'.

	self assert: generator asString equals: '":L1",' 
]

{ #category : #tests }
ASZigCompileMethodGeneratorTest >> testZigFileGenerator_pushLiteral [

	| generator |
	generator := ASZigThreadedOutput new testing.
	generator pushLiteral: 10.

	self 
		assert: generator asString
		equals: '&e.pushLiteral,Object.from(10),'
]

{ #category : #tests }
ASZigCompileMethodGeneratorTest >> testZigFileGenerator_pushLocal [

	| output selfIndex |
	selfIndex := 5.
	output := ASZigThreadedOutput new testing.
	output codeGenerator
		locals: #( foo bar )
		arguments: #( blat fubar ).
	output pushLocal: #self.

	self
		assert: output asString
		equals: '&e.pushLocal,' , selfIndex asString , ', // #self' 
]

{ #category : #tests }
ASZigCompileMethodGeneratorTest >> testZigFileGenerator_pushReturn [

	| generator |
	generator := ASZigThreadedOutput new testing.
	generator returnTop.

	self assert: generator asString equals: '&e.returnTop,' 
]

{ #category : #tests }
ASZigCompileMethodGeneratorTest >> testZigFileGenerator_pushSend [

	| generator aSelector numArgs |
	aSelector := #mySelector:other:.
	numArgs := aSelector numArgs.
	generator := ASZigThreadedOutput testing.
	generator send: aSelector useCall: false.

	self
		assert: generator asString
		equals: '&e.send' , numArgs asString , ',' , aSelector , ',' 
]
