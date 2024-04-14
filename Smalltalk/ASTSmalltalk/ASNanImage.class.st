Class {
	#name : 'ASNanImage',
	#superclass : 'ASImage',
	#category : 'ASTSmalltalk-Image',
	#package : 'ASTSmalltalk',
	#tag : 'Image'
}

{ #category : 'adding' }
ASNanImage >> double: n [

	^ ('16r' , n hex) asNumber
]

{ #category : 'adding' }
ASNanImage >> literal: l class: c [

	^ 16rFFF0000000000000 + l + (c bitShift: 32)
]

{ #category : 'adding' }
ASNanImage >> pointer: p class: c [

	^ 16rFFF7000000000000 + p
]

{ #category : 'adding' }
ASNanImage >> smallInteger: n [

	^ 16rFFFC000000000000 + n
]
