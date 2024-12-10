Class {
	#name : 'ASZigTest',
	#superclass : 'ASCodeTest',
	#category : 'Zag-Core-Tests-Tests',
	#package : 'Zag-Core-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
ASZigTest >> setUp [

	super setUp.
	file := ASZigFileContext new
]
