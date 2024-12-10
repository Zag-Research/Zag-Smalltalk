Class {
	#name : 'ASSourceCompilerTest',
	#superclass : 'TestCase',
	#category : 'Zag-Core-Tests-Tests',
	#package : 'Zag-Core-Tests',
	#tag : 'Tests'
}

{ #category : 'asserting' }
ASSourceCompilerTest >> assertEquivalent: anObject [ 
	| result |
	result := ASSourceCompiler new statementForTest: anObject printString.
	self assert: result baseValue equals: anObject.

]

{ #category : 'tests' }
ASSourceCompilerTest >> testCascade [
	| method |
	method := ASSourceCompiler new compileMethod: ASCompileTestClass1>>#cascade.
	self assert: method arguments equals: #().
	self assert: method selector equals: #cascade.
	self assert: method scope asArray equals: #(foo).
	self assert: method body size equals: 17.

]

{ #category : 'tests' }
ASSourceCompilerTest >> testGives43 [
	| method |
	method := ASSourceCompiler new compileMethod: ASCompileTestClass1>>#gives43.
	self assert: method arguments equals: #().
	self assert: method selector equals: #gives43.
	self assert: method scope asArray equals: #(forty2).
	self assert: method body size equals: 2.

]

{ #category : 'tests' }
ASSourceCompilerTest >> testOr [
	| method |
	method := ASSourceCompiler new compileMethod: ASCompileTestClass1>>#|.
	self assert: method arguments equals: #(n1).
	self assert: method selector equals: #|.
	self assert: method scope asArray equals: #(n1 temp).
	self assert: method body size equals: 2.

]

{ #category : 'tests' }
ASSourceCompilerTest >> testOrOr [
	| method |
	method := ASSourceCompiler new compileMethod: ASCompileTestClass1>>#||.
	self assert: method arguments equals: #(n1).
	self assert: method selector equals: #||.

]

{ #category : 'tests' }
ASSourceCompilerTest >> testPlus [
	| method |
	method := ASSourceCompiler new compileMethod: ASCompileTestClass1>>#+.
	self assert: method arguments equals: #(n1).
	self assert: method selector equals: #+.

]

{ #category : 'tests' }
ASSourceCompilerTest >> testPlus1 [
	| method |
	method := ASSourceCompiler new compileMethod: ASCompileTestClass1>>#plus1:.
	self assert: method arguments equals: #(aNumber).
	self assert: method selector equals: #plus1:.

]

{ #category : 'tests' }
ASSourceCompilerTest >> testPlusAndAnd [
	| method |
	method := ASSourceCompiler new compileMethod: ASCompileTestClass1>>#plus:and:and:.
	self assert: method arguments equals: #(n1 n2 n3).
	self assert: method selector equals: #plus:and:and:.

]

{ #category : 'tests' }
ASSourceCompilerTest >> testSimple1 [
	| method |
	method := ASSourceCompiler new compileMethod: ASCompileTestClass1>>#simple1.
	self assert: method arguments equals: #().
	self assert: method selector equals: #simple1.

]

{ #category : 'tests' }
ASSourceCompilerTest >> testStatements [
	self assertEquivalent: 42.
	self assertEquivalent: #hello.
	self assertEquivalent: 'abc'.
	self assertEquivalent: nil.
	self assertEquivalent: #( 1 2 3).
	self assertEquivalent: #( 1 (2 3) #(nil nil)).

]

{ #category : 'tests' }
ASSourceCompilerTest >> testUnaries [
	| method |
	method := ASSourceCompiler new compileMethod: ASCompileTestClass1>>#unaries.
	self assert: method arguments equals: #().
	self assert: method selector equals: #unaries.

]
