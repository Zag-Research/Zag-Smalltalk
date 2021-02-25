Class {
	#name : #ASCompileTest,
	#superclass : #TestCase,
	#category : #'ASTSmalltalk-Tests-Tests'
}

{ #category : #tests }
ASCompileTest >> testSimple1 [
	self assert: (ASCompile new compileMethod: ASCompileTestClass1>>#simple1) equals: (ASMethodNode with: {}).

]
