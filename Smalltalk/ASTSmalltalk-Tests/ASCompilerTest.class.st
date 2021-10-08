Class {
	#name : #ASCompilerTest,
	#superclass : #TestCase,
	#category : #'ASTSmalltalk-Tests-Tests'
}

{ #category : #tests }
ASCompilerTest >> testOr [
	| method |
	method := ASCompiler new compileMethod: ASCompileTestClass1>>#|.
	self assert: method arguments equals: #().
	self assert: method selector equals: #|.

]

{ #category : #tests }
ASCompilerTest >> testPlus [
	| method |
	method := ASCompiler new compileMethod: ASCompileTestClass1>>#+.
	self assert: method arguments equals: #().
	self assert: method selector equals: #+.

]

{ #category : #tests }
ASCompilerTest >> testPlus1 [
	| method |
	method := ASCompiler new compileMethod: ASCompileTestClass1>>#plus1:.
	self assert: method arguments equals: #().
	self assert: method selector equals: #plus1:.

]

{ #category : #tests }
ASCompilerTest >> testPlusAndAnd [
	| method |
	method := ASCompiler new compileMethod: ASCompileTestClass1>>#plus:and:and:.
	self assert: method arguments equals: #().
	self assert: method selector equals: #plus:and:and:.

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

]
