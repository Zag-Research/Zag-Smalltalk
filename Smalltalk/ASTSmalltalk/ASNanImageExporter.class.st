"
I have the knowledge of how to encode values for NaN images
"
Class {
	#name : 'ASNanImageExporter',
	#superclass : 'ASImageExporter',
	#classVars : [
		'GroupHeap',
		'GroupImmediates',
		'GroupSmallInt0'
	],
	#category : 'ASTSmalltalk-Image',
	#package : 'ASTSmalltalk',
	#tag : 'Image'
}

{ #category : 'accessing' }
ASNanImageExporter class >> boolean: value [

	^ self literal: value asBit class: ClassFalse + value asBit
]

{ #category : 'converting' }
ASNanImageExporter class >> double: n for: ignored [

	^ ('16r' , n hex) asNumber
]

{ #category : 'class initialization' }
ASNanImageExporter class >> initialize [

	GroupHeap := 16rFFF7000000000000.
	GroupSmallInt0 := 16rFFFC000000000000.
	GroupImmediates := 16rFFF0000000000000
]

{ #category : 'converting' }
ASNanImageExporter class >> literal: l class: c [

	^ GroupImmediates + l + (c bitShift: 32)
]

{ #category : 'converting' }
ASNanImageExporter class >> pointer: p [

	^ GroupHeap + p
]

{ #category : 'converting' }
ASNanImageExporter class >> smallInteger: n [

	^ GroupSmallInt0 + n
]

{ #category : 'converting' }
ASNanImageExporter class >> undefinedObject [

	^ self literal: 16rffffffff class: ClassUndefinedObject
]

{ #category : 'converting' }
ASNanImageExporter class >> zagFalse [
	
	^ self literal: 0 class: 4
]

{ #category : 'converting' }
ASNanImageExporter class >> zagNil [

	^ self literal: 16rffffffff class: 3
]

{ #category : 'converting' }
ASNanImageExporter class >> zagTrue [

	^ self literal: 1 class: 5
]
