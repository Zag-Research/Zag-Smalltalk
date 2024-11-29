"
I am storage that will be exported to a .image file that will be loaded by a Zag runtime
"
Class {
	#name : 'ASImageHeader',
	#superclass : 'ASStorage',
	#category : 'ASTSmalltalk-Image',
	#package : 'ASTSmalltalk',
	#tag : 'Image'
}

{ #category : 'accessing' }
ASImageHeader class >> extension [

	^ '.image'
]

{ #category : 'initialization' }
ASImageHeader >> initializeStorage [

	freeIndex := 1
]
