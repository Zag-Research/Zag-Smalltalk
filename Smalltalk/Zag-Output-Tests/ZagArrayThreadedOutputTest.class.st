Class {
	#name : 'ZagArrayThreadedOutputTest',
	#superclass : 'ASCodeTest',
	#instVars : [
		'output'
	],
	#category : 'Zag-Output-Tests-Tests',
	#package : 'Zag-Output-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
ZagArrayThreadedOutputTest >> setUp [

	super setUp.
	output := ZagThreadedOutput new
]

{ #category : 'tests' }
ZagArrayThreadedOutputTest >> test_addTwoNumbers [

	self
		assert:
		(output contentsForMethod: ASCompileTestClass1 >> #addTwoNumbers)
		equals: #( #( label #'ASCompileTestClass1.addTwoNumbers' )
			   pushContext #( method ) pushLiteral 4 pushLiteral 5 send #+ #( label
			      #'ASCompileTestClass1.addTwoNumbers.1' )
			   storeLocal 2 popLocal 1 pushLocal 2 pushLocal 1 tailSend
			   #+ )
]

{ #category : 'tests' }
ZagArrayThreadedOutputTest >> test_classVar [

	| reference |
	reference := ASCompileTestClass1 classPool associationAt: #Foo.
	ASCompileTestClass1 classVar: 42.
	self
		assert: (output contentsForMethod: ASCompileTestClass1 >> #classVar)
		equals: { #( label #'ASCompileTestClass1.classVar' ) .
			   #pushAssociationValue . { #object . reference } . #returnTop }
]

{ #category : 'tests' }
ZagArrayThreadedOutputTest >> test_fib [

	self
		assert: (output contentsForMethod: ASCompileTestClass1 >> #fib:)
		equals: '&p.p1,&e.pushLiteral,False,&e.returnTop,'
]

{ #category : 'tests' }
ZagArrayThreadedOutputTest >> test_prim [

	self
		assert: (output contentsForMethod: ASCompileTestClass1 >> #prim)
		equals: #( #( label #'ASCompileTestClass1.prim' )
			   #( primitive 1 ) pushLiteral false returnTop )
]

{ #category : 'tests' }
ZagArrayThreadedOutputTest >> test_simple1 [

	self
		assert: (output contentsForMethod: ASCompileTestClass1 >> #simple1)
		equals: #( #( label #'ASCompileTestClass1.simple1' )
			   pushLiteral 42 returnTop )
]

{ #category : 'tests' }
ZagArrayThreadedOutputTest >> test_simple1_noreturn [

	self
		assert:
		(output contentsForMethod: ASCompileTestClass1 >> #simple1_noreturn)
		equals: #( #( label #'ASCompileTestClass1.simple1_noreturn' )
			   pushLiteral 42 drop returnSelf )
]
