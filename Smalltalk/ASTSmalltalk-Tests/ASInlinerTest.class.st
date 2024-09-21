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
