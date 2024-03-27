"
I generate runtime compile objects
"
Class {
	#name : 'ASImageOutput',
	#superclass : 'ASOutput',
	#classTraits : 'ASOutputAPI classTrait',
	#instVars : [
		'image'
	],
	#pools : [
		'ASZagConstants'
	],
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'accessing' }
ASImageOutput >> context: aContext [

	image := aContext image.
	^ super context: aContext
]

{ #category : 'accessing' }
ASImageOutput >> name [

	^ 'iMaGe'
]
