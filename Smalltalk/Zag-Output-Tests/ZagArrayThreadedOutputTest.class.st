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
		'&e.verifySelector,&e.pushContext,"^",":MaddTwoNumbers:1",&e.pushLiteral,Object.from(4),&e.pushLiteral,Object.from(5),&e.send1,Sym.@"+",":MaddTwoNumbers:2",&e.storeLocal,1,&e.popLocal,0,&e.pushLocal,1,&e.pushLocal,0,&e.send1tail,Sym.@"+",'

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
