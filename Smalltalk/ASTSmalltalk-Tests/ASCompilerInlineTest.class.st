Class {
	#name : #ASCompilerInlineTest,
	#superclass : #TestCase,
	#category : #'ASTSmalltalk-Tests-Tests'
}

{ #category : #asserting }
ASCompilerInlineTest >> assert: aMethod inlinesTo: ast [

	| result |
	result := ASPharoCompiler new compileMethod: aMethod.
	result inlineMethod.
	self assert: result equals: ast
]

{ #category : #tests }
ASCompilerInlineTest >> testGives43 [

	self
		assert: ASCompileTestClass1 >> #gives43
		inlinesTo: (ASMethodNode
				 class: ASCompileTestClass1
				 selector: #gives43
				 locals: #( #forty2 )
				 body: { 
						 (ASAssign variable: #forty2 expression: (ASSend
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
										    target: (ASLocal name: #forty2)
										    selector: #-
										    args: { (ASLiteral literal: 4) }) })) })
]
