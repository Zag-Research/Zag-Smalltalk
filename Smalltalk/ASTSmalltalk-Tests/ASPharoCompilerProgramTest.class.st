Class {
	#name : #ASPharoCompilerProgramTest,
	#superclass : #TestCase,
	#category : #'ASTSmalltalk-Tests-Tests'
}

{ #category : #compiling }
ASPharoCompilerProgramTest >> buildFromClass: class method: method [

	^ ASPharoCompiler new buildFromClass: class method: method
]

{ #category : #tests }
ASPharoCompilerProgramTest >> testStart42 [

	| result |
	result := self buildFromClass: ASCompileTestClass1 method: #start42.
	self assert: result classes asSet equals: { ASCompileTestClass1 } asSet.
]
