"
I output to an image
"
Class {
	#name : 'ASImageOutput',
	#superclass : 'ASOutput',
	#instVars : [
		'image'
	],
	#category : 'Zag-Output-Image-Output',
	#package : 'Zag-Output',
	#tag : 'Image-Output'
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
