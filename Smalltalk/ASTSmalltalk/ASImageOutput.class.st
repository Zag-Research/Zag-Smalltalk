"
I output to an image
"
Class {
	#name : 'ASImageOutput',
	#superclass : 'ASOutput',
	#instVars : [
		'image'
	],
	#pools : [
		'AnObsoleteZagConstants'
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
