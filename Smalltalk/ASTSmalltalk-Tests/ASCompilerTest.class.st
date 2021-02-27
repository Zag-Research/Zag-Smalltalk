Class {
	#name : #ASCompilerTest,
	#superclass : #TestCase,
	#category : #'ASTSmalltalk-Tests-Tests'
}

{ #category : #tests }
ASCompilerTest >> testSimple1 [
	self assert: (ASCompiler new compileMethod: ASCompileTestClass1>>#simple1) equals: (ASMethodNode with: {}).

]
