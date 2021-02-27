Class {
	#name : #ASCompilerTest,
	#superclass : #TestCase,
	#category : #'ASTSmalltalk-Tests-Tests'
}

{ #category : #tests }
ASCompilerTest >> testSimple1 [
	| method |
	method := ASCompiler new compileMethod: ASCompileTestClass1>>#simple1.
	self assert: method arguments equals: #().
	self assert: method selector equals: #simple1.

]
