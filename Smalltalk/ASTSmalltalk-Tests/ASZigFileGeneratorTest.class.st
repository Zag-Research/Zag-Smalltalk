Class {
	#name : #ASZigFileGeneratorTest,
	#superclass : #TestCase,
	#category : #'ASTSmalltalk-Tests-Tests'
}

{ #category : #tests }
ASZigFileGeneratorTest >> testZigFileGenerator_addComment [

	| generator comment |
	comment := 'this is a comment!'.
	generator := ASZigCompileMethodGenerator new.
	generator comment: comment.

	self assert: (generator asString) equals: ' // ''' , comment, ''''
]

{ #category : #tests }
ASZigFileGeneratorTest >> testZigFileGenerator_goTo [

	| generator |
	generator := ASZigCompileMethodGenerator new.
	generator goTo: #hello.

	self assert: (generator asString) equals: '"hello"'
]

{ #category : #tests }
ASZigFileGeneratorTest >> testZigFileGenerator_pushDrop [

	| generator |
	generator := ASZigCompileMethodGenerator new.
	generator pushDrop.

	self assert: (generator asString) equals: '&e.drop,'
]

{ #category : #tests }
ASZigFileGeneratorTest >> testZigFileGenerator_pushLabel [

	| generator |
	generator := ASZigCompileMethodGenerator new.
	self assert: generator nextLabel equals: 'L1'.

	self assert: generator asString equals: '":L1",'
]

{ #category : #tests }
ASZigFileGeneratorTest >> testZigFileGenerator_pushLiteral [

	| generator |
	generator := ASZigCompileMethodGenerator new.
	generator pushLiteral: 10.

	self
		assert: (generator asString)
		equals: '&e.pushLiteral, Object.from(10),'
]

{ #category : #tests }
ASZigFileGeneratorTest >> testZigFileGenerator_pushLocal [

	| generator selfIndex |
	selfIndex := 5.
	generator := ASZigCompileMethodGenerator new.
	generator pushLocal: #self index: selfIndex.

	self assert: generator instructions size equals: 2.
	self
		assert: (generator instructions at: 1)
		equals: '&e.pushLocal, ' , selfIndex asString , ','.
	self assert: (generator instructions at: 2) equals: '// self'
]

{ #category : #tests }
ASZigFileGeneratorTest >> testZigFileGenerator_pushReturn [

	| generator |
	generator := ASZigCompileMethodGenerator new.
	generator pushReturn.

	self assert: (generator asString ) equals: '&e.returnTop,'
]

{ #category : #tests }
ASZigFileGeneratorTest >> testZigFileGenerator_pushSend [

	| generator aSelector numArgs |
	aSelector := #mySelector:other:.
	numArgs := aSelector numArgs.
	generator := ASZigCompileMethodGenerator new.
	generator pushSendForSelector: aSelector.

	self
		assert: generator asString
		equals: '&e.send' , numArgs asString , ', ' , aSelector , ','
]
