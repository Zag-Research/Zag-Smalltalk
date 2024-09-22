Class {
	#name : 'ASZigTest',
	#superclass : 'ASCodeTest',
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
ASZigTest >> setUp [

	super setUp.
	file := ASZigFileContext new
]
