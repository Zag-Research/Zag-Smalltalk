Class {
	#name : 'ASCodeTests',
	#superclass : 'TestCase',
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'method' }
ASCodeTests >> astFor: aMethod [ 
	^ ASPharoCompiler new astFor: aMethod
]
