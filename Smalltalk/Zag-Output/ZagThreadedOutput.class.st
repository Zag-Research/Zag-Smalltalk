"
I generate threaded output either to an image, into the current image, or as an array for tests
"
Class {
	#name : 'ZagThreadedOutput',
	#superclass : 'ASImageOutput',
	#instVars : [
		'stream',
		'selectors'
	],
	#category : 'Zag-Output-Image-Output',
	#package : 'Zag-Output',
	#tag : 'Image-Output'
}

{ #category : 'accessing' }
ZagThreadedOutput >> contents [

	^ stream contents
]

{ #category : 'reflective operations' }
ZagThreadedOutput >> doesNotUnderstand: aMessage [

	| selector |
	selector := aMessage selector.
	selector numArgs = 0 ifTrue: [
		 ZagImageOutputAPI methodDictionary at: selector ifPresent: [
		^ stream nextPut: selector ]].
	^ super doesNotUnderstand: aMessage
]

{ #category : 'initialization' }
ZagThreadedOutput >> initialize [

	super initialize.
	stream := #() writeStream.
	selectors := IdentitySet new
]

{ #category : 'API' }
ZagThreadedOutput >> literal: anObject [

	^ stream nextPut: anObject
]

{ #category : 'code generation' }
ZagThreadedOutput >> outputBasicBlockEnd [

	
]

{ #category : 'code generation' }
ZagThreadedOutput >> outputMethodDefinition: aString methods: aCollection [ 

]

{ #category : 'code generation' }
ZagThreadedOutput >> outputMethodStart: aBasicBlock [

	^ stream nextPut: { #label . aBasicBlock label asSymbol }
]

{ #category : 'writing' }
ZagThreadedOutput >> primitive: anInteger [

	^ stream nextPut: { #primitive . anInteger }
]

{ #category : 'threaded words' }
ZagThreadedOutput >> pushContext [

	^ stream nextPut: #pushContext; nextPut: #(method)
]

{ #category : 'accessing' }
ZagThreadedOutput >> reference: aString [

	^ stream nextPut: { #reference . aString asSymbol }
]
