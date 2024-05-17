Class {
	#name : 'ASZigTests',
	#superclass : 'ASCodeTests',
	#instVars : [
		'file'
	],
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
ASZigTests >> setUp [

	super setUp.
	file := ASZigFileContext new
]
