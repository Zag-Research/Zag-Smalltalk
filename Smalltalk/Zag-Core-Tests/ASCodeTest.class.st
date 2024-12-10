Class {
	#name : 'ASCodeTest',
	#superclass : 'TestCase',
	#instVars : [
		'file'
	],
	#category : 'Zag-Core-Tests-Tests',
	#package : 'Zag-Core-Tests',
	#tag : 'Tests'
}

{ #category : 'method' }
ASCodeTest >> astFor: aMethod [ 
	^ ASPharoCompiler new astFor: aMethod
]
