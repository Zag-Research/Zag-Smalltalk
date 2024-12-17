Class {
	#name : 'ASZigTest',
	#superclass : 'ASCodeTest',
	#category : 'Zag-Output-Tests-Tests',
	#package : 'Zag-Output-Tests',
	#tag : 'Tests'
}

{ #category : 'running' }
ASZigTest >> setUp [

	super setUp.
	file := ASZigFileContext new
]
