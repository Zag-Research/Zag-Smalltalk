Class {
	#name : 'ASCodeTest',
	#superclass : 'TestCase',
	#instVars : [
		'file'
	],
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'method' }
ASCodeTest >> astFor: aMethod [ 
	^ ASPharoCompiler new astFor: aMethod
]
