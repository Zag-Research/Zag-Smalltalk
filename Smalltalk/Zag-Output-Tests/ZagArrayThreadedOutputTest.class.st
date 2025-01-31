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

	| ast |
	ast := self astFor: ASCompileTestClass1 >> #addTwoNumbers.
	output generateCode: ast.
	self
		assert: output contents
		equals:
		#(#(label #'ASCompileTestClass1.addTwoNumbers') pushContext #(method) pushLiteral 4 pushLiteral 5 send #+ #(label #'ASCompileTestClass1.addTwoNumbers.1') storeLocal 2 popLocal 1 pushLocal 2 pushLocal 1 tailSend #+)
]

{ #category : 'tests' }
ZagArrayThreadedOutputTest >> test_fib [

	| ast |
	ast := ASPharoCompiler new astFor: ASCompileTestClass1 >> #fib:.
	self assert: output contents equals: '&p.p1,&e.pushLiteral,False,&e.returnTop,' 
]

{ #category : 'tests' }
ZagArrayThreadedOutputTest >> test_prim [

	| ast |
	ast := self astFor: ASCompileTestClass1 >> #prim.
	output
		halt;
		generateCode: ast.
	self
		assert: output contents
		equals: #( #( label #'ASCompileTestClass1.prim' )
			   #( primitive 1 ) pushLiteral false returnTop )
]

{ #category : 'tests' }
ZagArrayThreadedOutputTest >> test_simple1 [

	| ast |
	ast := self astFor: ASCompileTestClass1 >> #simple1.
	output generateCode: ast.
	self assert: output contents equals: #( pushLiteral 42 returnTop )
]

{ #category : 'tests' }
ZagArrayThreadedOutputTest >> test_simple1_noreturn [

	| ast |
	ast := self astFor: ASCompileTestClass1 >> #simple1_noreturn.
	output
		halt;
		generateCode: ast.
	self
		assert: output contents
		equals: #( #( label #'ASCompileTestClass1.simple1_noreturn' )
			    pushLiteral 42 drop returnSelf )
]
