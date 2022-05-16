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
		printsAs: 'ASLiteral literal: 42'.
	self
		assert: ASReturn
		instancesEqualAfter: [ :obj | obj expression: 42 ]
		printsAs: 'ASReturn expression: (42)'.
	self
		assert: ASRef
		instancesEqualAfter: [ :obj | obj variable: #a ]
		printsAs: 'ASRef variable: #a'.
	self
		assert: ASAssign
		instancesEqualAfter: [ :obj | obj variable: #a expression: 42 ]
		printsAs: 'ASAssign variable: #a expression: (42)'.
	self
		assert: ASMethodNode
		instancesEqualAfter: [ :obj | 
			obj
				class: Object;
				selector: #a ]
		printsAs: 'ASMethodNode class: Object selector: #a body: {}'.
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
		'ASMethodNode class: Object selector: #a arguments: #(#a1 #a2 #a3) locals: #(#l1 #l2) body: {17 . 42}'
]

{ #category : #tests }
ASPharoCompilerTest >> testCascade [

	self assert: ASCompileTestClass1 >> #cascade compilesTo: nil
]

{ #category : #tests }
ASPharoCompilerTest >> testGives43 [

	self assert: ASCompileTestClass1 >> #gives43 compilesTo: (ASMethodNode class: ASCompileTestClass1 selector: #gives43 locals: #(#forty2) body: {ASAssign variable: #forty2 expression: (ASSend target: (ASSend target: (ASSelf method: #gives43) selector: #unaries args: {}) selector: #* args: {ASLiteral literal: 2}) . ASReturn expression: (ASSend target: (ASSelf method: #gives43) selector: #plus1: args: {ASSend target: (ASRef variable: #forty2) selector: #- args: {ASLiteral literal: 4}})})
]

{ #category : #tests }
ASPharoCompilerTest >> testOr [

	self assert: ASCompileTestClass1 >> #| compilesTo: (ASMethodNode class: ASCompileTestClass1 selector: #| arguments: #(#n1) locals: #(#temp) body: {ASAssign variable: #temp expression: (ASSend target: (ASSelf method: #|) selector: #| args: {ASRef variable: #n1}) . ASReturn expression: (ASRef variable: #temp)})
]

{ #category : #tests }
ASPharoCompilerTest >> testOrOr [

	self assert: ASCompileTestClass1 >> #'||' compilesTo: (ASMethodNode class: ASCompileTestClass1 selector: #'||' arguments: #(#n1) locals: #(#temp #temp2) body: {ASAssign variable: #temp expression: (ASAssign variable: #temp2 expression: (ASSend target: (ASSelf method: #'||') selector: #'||' args: {ASRef variable: #n1})) . ASReturn expression: (ASRef variable: #temp)})
]

{ #category : #tests }
ASPharoCompilerTest >> testPlus [

	self assert: ASCompileTestClass1 >> #+ compilesTo: (ASMethodNode
			 class: ASCompileTestClass1
			 selector: #+
			 arguments: #( n1 )
			 locals: #( temp )
			 body: { 
					 (ASAssign variable: #temp expression: (ASSend
							   target: (ASSelf method: #+)
							   selector: #+ args: { (ASRef variable: #n1) })).
					 (ASReturn expression: (ASRef variable: #temp)) })
]

{ #category : #tests }
ASPharoCompilerTest >> testPlus1 [

	self
		assert: ASCompileTestClass1 >> #plus1:
		compilesTo: (ASMethodNode new
				 class: ASCompileTestClass1;
				 selector: #plus1:;
				 arguments: #( aNumber );
				 body: { (ASReturn new expression: (ASSend new
								   target: (ASRef new variable: #aNumber);
								   selector: #+ args: { (ASLiteral new literal: 1) })) })
]

{ #category : #tests }
ASPharoCompilerTest >> testPlusAndAnd [

	self
		assert: ASCompileTestClass1 >> #plus:and:and:
		compilesTo: (ASMethodNode new
				 class: ASCompileTestClass1;
				 selector: #plus:and:and:;
				 arguments: #( n1 n2 n3 );
				 body: { (ASReturn new expression: (ASSend new
								   target: (ASSend new
										    target: (ASRef new variable: #n1);
										    selector: #+ args: { (ASRef new variable: #n2) });
								   selector: #+ args: { (ASRef new variable: #n3) })) })
]

{ #category : #tests }
ASPharoCompilerTest >> testSimple1 [

	self
		assert: ASCompileTestClass1 >> #simple1
		compilesTo: (ASMethodNode new
				 class: ASCompileTestClass1;
				 selector: #simple1;
				 body: { (ASReturn new expression: (ASLiteral new literal: 42)) })
]

{ #category : #tests }
ASPharoCompilerTest >> testUnaries [
	| method |
	method := ASSourceCompiler new compileMethod: ASCompileTestClass1>>#unaries.
	self assert: method arguments equals: #().
	self assert: method selector equals: #unaries.

]
