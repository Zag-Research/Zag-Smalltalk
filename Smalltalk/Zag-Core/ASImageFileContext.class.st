"
I am a writer of .zag files.

I maintain a symbol table. The code generators will tell me about any symbols that need to be added.

When asked to output, I output an image file.
"
Class {
	#name : 'ASImageFileContext',
	#superclass : 'ASFileContext',
	#instVars : [
		'image'
	],
	#category : 'Zag-Core-Output',
	#package : 'Zag-Core',
	#tag : 'Output'
}

{ #category : 'instance creation' }
ASImageFileContext class >> with: anImage [

	^ self new image: anImage
]

{ #category : 'accessing' }
ASImageFileContext >> image [
	^ image
]

{ #category : 'accessing' }
ASImageFileContext >> image: anASImage [ 
	image := anASImage
]
