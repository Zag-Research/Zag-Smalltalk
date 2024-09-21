Class {
	#name : 'ASPharoCompilerTest',
	#superclass : 'TestCase',
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'asserting' }
ASPharoCompilerTest >> assert: aMethod compilesTo: ast [
	| result |
	result := ASPharoCompiler new compileMethod: aMethod.
	self assert: result equals: ast.

]

{ #category : 'tests' }
ASPharoCompilerTest >> assert: class instancesEqualAfter: block printsAs: printString [

	| obj1 obj2 |
	obj1 := class new.
	obj2 := class new.
	block value: obj1.
	block value: obj2.
	self assert: obj1 equals: obj2.
	self assert: obj1 printString equals: printString
]

{ #category : 'tests' }
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
		assert: ASLocal
		instancesEqualAfter: [ :obj | obj name: #a ]
		printsAs: 'ASLocal a'.
	self
		assert: ASAssign
		instancesEqualAfter: [ :obj |
		obj variable: ASLocal a expression: 42 ]
		printsAs: 'ASAssign variable: ASLocal a expression: (42)'.
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
				add: 17;
				add: 42 ]
		printsAs:
		'ASMethodNode class: Object selector: #a arguments: #(a1 a2 a3) locals: #(l1 l2) body: {17 . 42}'
]

{ #category : 'tests' }
ASPharoCompilerTest >> testBlock [

	self
		assert: ASCompileTestClass1 >> #block
		compilesTo:
			(ASMethodNode
				 selector: #block
				 body: { (ASReturn expression: (ASSend
							   target: (ASArray statements: {
										    (ASSend
											     target: (ASLiteral literal: 3)
											     selector: #+
											     args: { (ASLiteral literal: 4) }).
										    (ASSend
											     target: (ASLiteral literal: 4)
											     selector: #factorial
											     args: {  }) })
							   selector: #collect:
							   args:
								   { (ASBlock
									    arguments: #( #each )
									    locals: #( #temp )
									    body: {
											    (ASAssign
												     variable: (ASLocal name: #temp)
												     expression: (ASSend
														      target:
														      (ASRef variable: #each)
														      selector: #+
														      args: { (ASLiteral literal: 5) })).
											    (ASSend
												     target: (ASRef variable: (ASLocal name: #temp))
												     selector: #+
												     args: { (ASLiteral literal: 3) }) }) })) }
						 asASSequence)
]

{ #category : 'tests' }
ASPharoCompilerTest >> testCascade [

	self
		assert: ASCompileTestClass1 >> #cascade
		compilesTo: (ASMethodNode
				 selector: #cascade
				 locals: #( #foo )
				 body: {
						 (ASAssign variable: ASLocal foo expression: (ASSend
								   target: (ASLiteral literal: #( #abc nil 3 ))
								   selector: #asOrderedCollection
								   args: {  })).
						 (ASCascade
							  target:
								  (ASCascade target: (ASRef variable: ASLocal foo) sends: {
										   (ASSend
											    selector: #add:
											    args:
											    { (ASSend
												     target: (ASLiteral literal: 4)
												     selector: #negated
												     args: {  }) }).
										   (ASSend selector: #yourself args: {  }) })
							  sends: {
									  (ASSend selector: #add: args: { (ASSend
											    target:
											    (ASSend
												     target: (ASLiteral literal: 3)
												     selector: #negated
												     args: {  })
											    selector: #+
											    args:
											    { (ASSend
												     target: (ASLiteral literal: 4)
												     selector: #abs
												     args: {  }) }) }).
									  (ASSend selector: #size args: {  }).
									  (ASSend selector: #negated args: {  }).
									  (ASSend selector: #+ args: { (ASLiteral literal: 4) }).
									  (ASSend
										   selector: #+
										   args:
										   { (ASSend
											    target: (ASLiteral literal: 3)
											    selector: #negated
											    args: {  }) }).
									  (ASSend selector: #abs args: {  }) }) } asASSequence)
]

{ #category : 'tests' }
ASPharoCompilerTest >> testClassVar [

	self
		assert: ASCompileTestClass1 >> #classVar
		compilesTo: (ASMethodNode
				 selector: #classVar
				 body:
					 { (ASReturn expression: (ASRef variable: #Foo)) }
						 asASSequence)
]

{ #category : 'tests' }
ASPharoCompilerTest >> testGives43 [

	self
		assert: ASCompileTestClass1 >> #gives43
		compilesTo: (ASMethodNode
				 selector: #gives43
				 locals: #( #forty2 )
				 body: {
						 (ASAssign variable: ASLocal forty2 expression: (ASSend
								   target: (ASSend
										    target: (ASSelf method: #gives43)
										    selector: #unaries
										    args: {  })
								   selector: #*
								   args: { (ASLiteral literal: 2) })).
						 (ASReturn expression: (ASSend
								   target: (ASSelf method: #gives43)
								   selector: #plus1:
								   args: { (ASSend
										    target: (ASRef variable: ASLocal forty2)
										    selector: #-
										    args: { (ASLiteral literal: 4) }) })) } asASSequence)
]

{ #category : 'tests' }
ASPharoCompilerTest >> testInstVar [

	self
		assert: ASCompileTestClass1 >> #instVar
		compilesTo: (ASMethodNode
				 selector: #instVar
				 body: { (ASReturn expression:
						  (ASRef variable: (ASCInstanceVariable name: #bar))) }
						 asASSequence)
]

{ #category : 'tests' }
ASPharoCompilerTest >> testObject [

	self
		assert: ASCompileTestClass1 >> #object
		compilesTo: (ASMethodNode
				 selector: #object
				 body: { (ASReturn expression: (ASSend
							   target: (ASRef variable: (ASCGlobalVariable name: #Object))
							   selector: #new
							   args: {  })) } asASSequence)
]

{ #category : 'tests' }
ASPharoCompilerTest >> testOr [

	self assert: ASCompileTestClass1 >> #| compilesTo: (ASMethodNode
			 selector: #|
			 arguments: #( #n1 )
			 locals: #( #temp )
			 body: {
					 (ASAssign variable: ASLocal temp expression: (ASSend
							   target: (ASSelf method: #|)
							   selector: #|
							   args: { (ASRef variable:  #n1) })).
					 (ASReturn expression: (ASRef variable: ASLocal temp)) }
					 asASSequence)
]

{ #category : 'tests' }
ASPharoCompilerTest >> testOrOr [

	self assert: ASCompileTestClass1 >> #'||' compilesTo: (ASMethodNode
			 selector: #'||'
			 arguments: #( #n1 )
			 locals: #( #temp #temp2 )
			 body: {
					 (ASAssign
						  variable: ASLocal temp
						  expression:
							  (ASAssign variable: ASLocal temp2 expression: (ASSend
									    target: (ASSelf method: #'||')
									    selector: #'||'
									    args: { (ASRef variable: #n1) }))).
					 (ASReturn expression: (ASRef variable: ASLocal temp)) }
					 asASSequence)
]

{ #category : 'tests' }
ASPharoCompilerTest >> testPlus [

	self assert: ASCompileTestClass1 >> #+ compilesTo: (ASMethodNode
			 selector: #+
			 arguments: #( n1 )
			 locals: #( temp )
			 body: {
					 (ASAssign variable: ASLocal temp expression: (ASSend
							   target: (ASSelf method: #+)
							   selector: #+
							   args: { (ASRef variable: #n1) })).
					 (ASReturn expression: (ASRef variable: ASLocal temp)) }
					 asASSequence)
]

{ #category : 'tests' }
ASPharoCompilerTest >> testPlus1 [

	self
		assert: ASCompileTestClass1 >> #plus1:
		compilesTo: (ASMethodNode new
				 class: ASCompileTestClass1;
				 selector: #plus1:;
				 arguments: #( aNumber );
				 body: { (ASReturn new expression: (ASSend new
								   target: (ASRef new variable: #aNumber);
								   selector: #+ args: { (ASLiteral new literal: 1) })) }
						 asASSequence)
]

{ #category : 'tests' }
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
										    selector: #+
										    args: { (ASRef new variable: #n2) });
								   selector: #+
								   args: { (ASRef new variable: #n3) })) }
						 asASSequence)
]

{ #category : 'tests' }
ASPharoCompilerTest >> testPool [
	" pool variable counts as a class variable "

	self assert: ASCompileTestClass3 >> #pool compilesTo: (ASMethodNode
			 selector: #pool
			 body:
				 { (ASReturn expression:
					  (ASRef variable: (#Foo))) } asASSequence)
]

{ #category : 'tests' }
ASPharoCompilerTest >> testSimple1 [

	self
		assert: ASCompileTestClass1 >> #simple1
		compilesTo: (ASMethodNode new
				 class: ASCompileTestClass1;
				 selector: #simple1;
				 body: { (ASReturn new expression: (ASLiteral new literal: 42)) } asASSequence)
]

{ #category : 'tests' }
ASPharoCompilerTest >> testSuper [

	self
		assert: ASCompileTestClass2 >> #classVar
		compilesTo: (ASMethodNode
				
				 selector: #classVar
				 body: { (ASReturn expression: (ASSend
							   target: (ASSend
									    target:
									    (ASSelf method: #classVar superclass: ASCompileTestClass1)
									    selector: #classVar
									    args: {  })
							   selector: #negated
							   args: {  })) } asASSequence)
]

{ #category : 'tests' }
ASPharoCompilerTest >> testUnaries [
	| method |
	method := ASPharoCompiler new compileMethod: ASCompileTestClass1>>#unaries.
	self assert: method arguments equals: #().
	self assert: method selector equals: #unaries.

]
