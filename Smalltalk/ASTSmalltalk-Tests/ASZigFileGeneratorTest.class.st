Class {
	#name : #ASZigFileGeneratorTest,
	#superclass : #TestCase,
	#category : #'ASTSmalltalk-Tests-Tests'
}

{ #category : #tests }
ASZigFileGeneratorTest >> testZigFileGenerator_addComment [

	| generator comment |
	comment := 'this is a comment!'.
	generator := ASZigGenerator new.
	generator comment: comment.

	self assert: (generator instructions at: 1) equals: '// ', comment
]

{ #category : #tests }
ASZigFileGeneratorTest >> testZigFileGenerator_goTo [

	| generator |
	generator := ASZigGenerator new.
	generator goTo: #hello.

	self assert: (generator instructions at: 1) equals: '":hello"'
]

{ #category : #tests }
ASZigFileGeneratorTest >> testZigFileGenerator_pushDrop [

	| generator |
	generator := ASZigGenerator new.
	generator pushDrop.

	self
		assert: (generator instructions at: 1)
		equals: '&e.drop,'
]

{ #category : #tests }
ASZigFileGeneratorTest >> testZigFileGenerator_pushLabel [

	| generator |
	generator := ASZigGenerator new.
	generator pushLabel: #hello.

	self assert: (generator instructions at: 1) equals: '":hello"'
]

{ #category : #tests }
ASZigFileGeneratorTest >> testZigFileGenerator_pushLiteral [

	| generator |
	generator := ASZigGenerator new.
	generator pushLiteral: 10.

	self assert: (generator instructions at: 1) equals: '&e.pushLiteral, Object.from(10),'
]

{ #category : #tests }
ASZigFileGeneratorTest >> testZigFileGenerator_pushLocal [

	| generator selfIndex |
	selfIndex := 5.
	generator := ASZigGenerator new.
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
	generator := ASZigGenerator new.
	generator pushReturn.

	self
		assert: (generator instructions at: 1)
		equals: '&e.returnTop,'
]

{ #category : #tests }
ASZigFileGeneratorTest >> testZigFileGenerator_pushSend [

	| generator aSelector numArgs |
	aSelector := #mySelector.
	numArgs := 5.
	generator := ASZigGenerator new.
	generator pushSendForSelector: aSelector numArgs: numArgs.

	self
		assert: (generator instructions at: 1)
		equals: '&e.send' , numArgs asString , ', ', aSelector, ','.
]
