"
I have the knowledge of how to encode values for NaN images
"
Class {
	#name : 'ASNanImage',
	#superclass : 'ASImage',
	#classVars : [
		'GroupHeap',
		'GroupImmediates',
		'GroupSmallInt0'
	],
	#category : 'ASTSmalltalk-Image',
	#package : 'ASTSmalltalk',
	#tag : 'Image'
}

{ #category : 'converting' }
ASNanImage class >> double: n [

	^ ('16r' , n hex) asNumber
]

{ #category : 'class initialization' }
ASNanImage class >> initialize [

	GroupHeap := 16rFFF7000000000000.
	GroupSmallInt0 := 16rFFFC000000000000.
	GroupImmediates := 16rFFF0000000000000
]

{ #category : 'converting' }
ASNanImage class >> literal: l class: c [

	^ GroupImmediates + l + (c bitShift: 32)
]

{ #category : 'converting' }
ASNanImage class >> pointer: p class: c [

	^ GroupHeap + p
]

{ #category : 'converting' }
ASNanImage class >> smallInteger: n [

	^ GroupSmallInt0 + n
]

{ #category : 'converting' }
ASNanImage class >> zagFalse [
	
	^ self literal: 0 class: 4
]

{ #category : 'converting' }
ASNanImage class >> zagNil [

	^ self literal: 16rffffffff class: 3
]

{ #category : 'converting' }
ASNanImage class >> zagTrue [

	^ self literal: 1 class: 5
]
