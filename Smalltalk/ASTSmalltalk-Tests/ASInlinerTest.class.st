Class {
	#name : 'ASInlinerTest',
	#superclass : 'TestCase',
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
ASInlinerTest >> setUp [
	super setUp.

	ASInliner reset
]

{ #category : 'tests' }
ASInlinerTest >> testBarInlined [
	"bar

	^ 42"

	| cg |
	cg := ASCodeGenerator
		      targetClass: ASCompileTestClass1
		      selector: #bar.
	cg
		inline: true;
		finalAnalysis;
		flowAnalysis.
	cg inspect
]

{ #category : 'tests' }
ASInlinerTest >> testBlockReturnEasyInline [
	"
blockReturn: p1
	true ifTrue: [ ^ 42 ]
"

	(ASNoOutput withInlining compileMethod:
		 ASCompileTestClass1 >> #blockReturnEasy) inspect
]

{ #category : 'tests' }
ASInlinerTest >> testBlockReturnInline [
	"
blockReturn: p1
	true ifTrue: [ ^ p1 ]
"

	| cg |
	cg := ASCodeGenerator
		      targetClass: ASCompileTestClass1
		      selector: #blockReturn:.
	cg
		inspect;
		halt.
	cg
		inline: true;
		finalAnalysis;
		inspect
]

{ #category : 'tests' }
ASInlinerTest >> testBlockValueInlined [
	"blockValue

	^ [ 4 ] value "

	| cg |
	cg := ASCodeGenerator
		      targetClass: ASCompileTestClass1
		      selector: #blockValue.
	cg
		inline: true;
		finalAnalysis;
		flowAnalysis.
	cg inspect.
]

{ #category : 'tests' }
ASInlinerTest >> testCollectionAnyOne [
	"blockValue

	^ [ 4 ] value "

	(ASNoOutput withInlining compileMethod: ASCompileTestClass1 >> #collectionAnyOne)
		inspect.
]

{ #category : 'tests' }
ASInlinerTest >> testFoo2Inlined [
	"foo
	^ self bar"

	"bar
	^ 42"

	| cg |
	cg := ASCodeGenerator
		      targetClass: ASCompileTestClass1
		      selector: #foo2.
	cg
		inline: true;
		finalAnalysis;
		inspect
]

{ #category : 'tests' }
ASInlinerTest >> testInitial [

	self assert: (ASInliner lookup: #printOn:) equals: nil.
	self deny: (ASInliner lookup: #ifTrue:ifFalse:) isPrimitive.
	self
		assert: (ASInliner lookup: #ifTrue:ifFalse:) methods asSet
		equals: {
				(ASTFalse >> #ifTrue:ifFalse:).
				(ASTTrue >> #ifTrue:ifFalse:) } asSet.
	self
		assert: (ASInliner lookup: #yourself class: SmallInteger) method
		equals: ASTObject >> #yourself.
	self assert:
		(ASInliner lookup: #'<=' class: SmallInteger) isPrimitive.
	self
		assert: (ASInliner lookup: #'<=' class: SmallInteger) method
		equals: ASTSmallInteger >> #'<='
]

{ #category : 'tests' }
ASInlinerTest >> testRecursiveMethod [
	"recursiveMethod
	^ self recursiveMethod"

	| cg |
	cg := ASCodeGenerator
		      targetClass: ASCompileTestClass1
		      selector: #recursiveMethod.
	cg
		inspect;
		halt.
	cg
		inline: true;
		finalAnalysis;
		inspect
]

{ #category : 'tests' }
ASInlinerTest >> testTailRecursiveMethod [
	"recursiveMethod
	^ self recursiveMethod"

	| cg |
	cg := ASCodeGenerator
		      targetClass: ASCompileTestClass1
		      selector: #tailRecursiveMethod.
	cg
		inspect;
		halt.
	cg
		inline: true;
		finalAnalysis;
		inspect
]

{ #category : 'tests' }
ASInlinerTest >> testWhileTrueInlined [
	"whileTrue

	| dirty |
	[
	dirty := false.
	dirty ] whileTrue"

	(ASNoOutput withInlining compileMethod: ASCompileTestClass1 >> #whileTrue) inspect
]
