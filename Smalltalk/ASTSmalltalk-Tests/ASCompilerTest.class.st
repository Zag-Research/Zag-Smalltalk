Class {
	#name : #ASCompilerTest,
	#superclass : #TestCase,
	#category : #'ASTSmalltalk-Tests-Tests'
}

{ #category : #tests }
ASCompilerTest >> testPlus1 [
	| method |
	method := ASCompiler new compileMethod: ASCompileTestClass1>>#plus1:.
	self assert: method arguments equals: #().
	self assert: method selector equals: #plus1:.

]

{ #category : #tests }
ASCompilerTest >> testSimple1 [
	| method |
	method := ASCompiler new compileMethod: ASCompileTestClass1>>#simple1.
	self assert: method arguments equals: #().
	self assert: method selector equals: #simple1.

]

{ #category : #tests }
ASCompilerTest >> testUnaries [
	| method |
	method := ASCompiler new compileMethod: ASCompileTestClass1>>#unaries.
	self assert: method arguments equals: #().
	self assert: method selector equals: #unaries.
	self halt.

]
