Class {
	#name : #ASPharoCompilerTest,
	#superclass : #TestCase,
	#category : #'ASTSmalltalk-Tests-Tests'
}

{ #category : #asserting }
ASPharoCompilerTest >> assert: aMethod compilesTo: ast [
	| result |
	result := ASPharoCompiler new compileMethod: aMethod.
	self assert: result equals: ast.

]

{ #category : #tests }
ASPharoCompilerTest >> assert: class instancesEqualAfter: block printsAs: printString [

	| obj1 obj2 |
	obj1 := class new.
	obj2 := class new.
	block value: obj1.
	block value: obj2.
	self assert: obj1 equals: obj2.
	self assert: obj1 printString equals: printString
]

{ #category : #tests }
ASPharoCompilerTest >> testASTNodesEqual [

	self
		assert: ASLiteral
		instancesEqualAfter: [ :obj | obj literal: 42 ]
		printsAs: 'ASLiteral new literal: 42'.
	self
		assert: ASReturn
		instancesEqualAfter: [ :obj | obj expression: 42 ]
		printsAs: 'ASReturn new expression: (42)'.
	self
		assert: ASRef
		instancesEqualAfter: [ :obj | obj variable: #a ]
		printsAs: 'ASRef new variable: #a'.
	self
		assert: ASAssign
		instancesEqualAfter: [ :obj | obj variable: #a expression: 42 ]
		printsAs: 'ASAssign new variable: #a expression: (42)'.
	self
		assert: ASMethodNode
		instancesEqualAfter: [ :obj | 
			obj
				class: Object;
				selector: #a ]
		printsAs: 'ASMethodNode new class: #Object; selector: #a;body: {}'.
	self
		assert: ASMethodNode
		instancesEqualAfter: [ :obj | 
			obj
				class: Object;
				selector: #a;
				arguments: #( a1 a2 a3 );
				locals: #( l1 l2 );
				addStatement: 17;
				addStatement: 42 ]
		printsAs:
		'ASMethodNode new class: #Object; selector: #a;arguments: #(#a1 #a2 #a3);locals: #(#l1 #l2);body: {17 . 42}'
]

{ #category : #tests }
ASPharoCompilerTest >> testCascade [
	| method |
	method := ASSourceCompiler new compileMethod: ASCompileTestClass1>>#cascade.
	self assert: method arguments equals: #().
	self assert: method selector equals: #cascade.
	self assert: method scope asArray equals: #(foo).
	self assert: method body size equals: 17.

]

{ #category : #tests }
ASPharoCompilerTest >> testGives43 [
	| method |
	method := ASSourceCompiler new compileMethod: ASCompileTestClass1>>#gives43.
	self assert: method arguments equals: #().
	self assert: method selector equals: #gives43.
	self assert: method scope asArray equals: #(forty2).
	self assert: method body size equals: 2.

]

{ #category : #tests }
ASPharoCompilerTest >> testOr [
	| method |
	method := ASSourceCompiler new compileMethod: ASCompileTestClass1>>#|.
	self assert: method arguments equals: #(n1).
	self assert: method selector equals: #|.
	self assert: method scope asArray equals: #(n1 temp).
	self assert: method body size equals: 2.

]

{ #category : #tests }
ASPharoCompilerTest >> testOrOr [
	| method |
	method := ASSourceCompiler new compileMethod: ASCompileTestClass1>>#||.
	self assert: method arguments equals: #(n1).
	self assert: method selector equals: #||.

]

{ #category : #tests }
ASPharoCompilerTest >> testPlus [
	| method |
	method := ASSourceCompiler new compileMethod: ASCompileTestClass1>>#+.
	self assert: method arguments equals: #(n1).
	self assert: method selector equals: #+.

]

{ #category : #tests }
ASPharoCompilerTest >> testPlus1 [
	| method |
	method := ASSourceCompiler new compileMethod: ASCompileTestClass1>>#plus1:.
	self assert: method arguments equals: #(aNumber).
	self assert: method selector equals: #plus1:.

]

{ #category : #tests }
ASPharoCompilerTest >> testPlusAndAnd [
	| method |
	method := ASSourceCompiler new compileMethod: ASCompileTestClass1>>#plus:and:and:.
	self assert: method arguments equals: #(n1 n2 n3).
	self assert: method selector equals: #plus:and:and:.

]

{ #category : #tests }
ASPharoCompilerTest >> testSimple1 [
	self assert: ASCompileTestClass1>>#simple1 compilesTo:
		(ASMethodNode )
]

{ #category : #tests }
ASPharoCompilerTest >> testStatements [
	self assertEquivalent: 42.
	self assertEquivalent: #hello.
	self assertEquivalent: 'abc'.
	self assertEquivalent: nil.
	self assertEquivalent: #( 1 (2 3) #(nil #nil)).

]

{ #category : #tests }
ASPharoCompilerTest >> testUnaries [
	| method |
	method := ASSourceCompiler new compileMethod: ASCompileTestClass1>>#unaries.
	self assert: method arguments equals: #().
	self assert: method selector equals: #unaries.

]
