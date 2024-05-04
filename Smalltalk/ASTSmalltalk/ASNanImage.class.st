"
I have the knowledge of how to encode values for NaN images
"
Class {
	#name : 'ASNanImage',
	#superclass : 'ASImage',
	#category : 'ASTSmalltalk-Image',
	#package : 'ASTSmalltalk',
	#tag : 'Image'
}

{ #category : 'as yet unclassified' }
ASNanImage class >> convertNumber: anInteger [

	anInteger isInteger ifTrue: [ ^ self smallInteger: anInteger ].
	self error: 'unexpected number'
]

{ #category : 'adding' }
ASNanImage class >> double: n [

	^ ('16r' , n hex) asNumber
]

{ #category : 'adding' }
ASNanImage class >> literal: l class: c [

	^ 16rFFF0000000000000 + l + (c bitShift: 32)
]

{ #category : 'adding' }
ASNanImage class >> pointer: p class: c [

	^ 16rFFF7000000000000 + p
]

{ #category : 'adding' }
ASNanImage class >> smallInteger: n [

	^ 16rFFFC000000000000 + n
]

{ #category : 'adding' }
ASNanImage >> double: n [ 
	^ self class double: n 
]

{ #category : 'adding' }
ASNanImage >> literal: l class: c [ 
	^ self class literal: l class: c 
]

{ #category : 'adding' }
ASNanImage >> pointer: p class: c [ 
	^ self class pointer: p class: c 
]

{ #category : 'adding' }
ASNanImage >> smallInteger: n [ 
	^ self class smallInteger: n 
]
