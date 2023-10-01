Class {
	#name : #ASZigCompileMethodGeneratorTest,
	#superclass : #TestCase,
	#category : #'ASTSmalltalk-Tests-Tests'
}

{ #category : #tests }
ASZigCompileMethodGeneratorTest >> testZigFileContext [

	| generator ast file |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #fib:.
	file := ASZigFileContext new.
	generator := ASZigCompileMethodGenerator new context: file;generateCodeFor: ast.

	self assert: generator name equals: 'Mfib:'
]

{ #category : #tests }
ASZigCompileMethodGeneratorTest >> testZigFileGenerator_addComment [

	| generator comment |
	comment := 'this is a comment!'.
	generator := ASZigCompileMethodGenerator new.
	generator comment: comment.

	self assert: (generator asString) equals: ' // ''' , comment, ''''
]

{ #category : #tests }
ASZigCompileMethodGeneratorTest >> testZigFileGenerator_goTo [

	| generator |
	generator := ASZigCompileMethodGenerator new.
	generator goTo: #hello.

	self assert: (generator asString) equals: '"hello"'
]

{ #category : #tests }
ASZigCompileMethodGeneratorTest >> testZigFileGenerator_pushDrop [

	| generator |
	generator := ASZigCompileMethodGenerator new.
	generator drop.

	self assert: (generator asString) equals: '&e.drop,'
]

{ #category : #tests }
ASZigCompileMethodGeneratorTest >> testZigFileGenerator_pushLabel [

	| generator |
	generator := ASZigCompileMethodGenerator new.
	self assert: generator nextLabel equals: 'L1'.

	self assert: generator asString equals: '":L1",'
]

{ #category : #tests }
ASZigCompileMethodGeneratorTest >> testZigFileGenerator_pushLiteral [

	| generator |
	generator := ASZigCompileMethodGenerator new.
	generator pushLiteral: 10.

	self
		assert: (generator asString)
		equals: '&e.pushLiteral, Object.from(10),'
]

{ #category : #tests }
ASZigCompileMethodGeneratorTest >> testZigFileGenerator_pushLocal [

	| generator selfIndex |
	selfIndex := 5.
	generator := ASZigCompileMethodGenerator new.
	generator locals: #(foo bar) arguments: #(blat fubar).
	generator pushLocal: #self.

	self
		assert: generator asString
		equals: '&e.pushLocal,', Character lf asString , selfIndex asString , ', // #self'
]

{ #category : #tests }
ASZigCompileMethodGeneratorTest >> testZigFileGenerator_pushReturn [

	| generator |
	generator := ASZigCompileMethodGenerator new.
	generator returnTop.

	self assert: (generator asString ) equals: '&e.returnTop,'
]

{ #category : #tests }
ASZigCompileMethodGeneratorTest >> testZigFileGenerator_pushSend [

	| generator aSelector numArgs |
	aSelector := #mySelector:other:.
	numArgs := aSelector numArgs.
	generator := ASZigCompileMethodGenerator new.
	generator send: aSelector.

	self
		assert: generator asString
		equals: '&e.send' , numArgs asString , ', ' , aSelector , ','
]
