"
I handle all the in-heap objects, except BlockClosures.

When first called, I ask the image writer to allocate space for me, and schedule me for processing, then I gather all the fields and indexable elements from the object.

When processing, I look up all the objects and get the appropriate heap object for them.

Finally I am called to write the fields so I output the header, a possible supplementary size, and all the values I accumulated.

This 3-phase process is (a) to handle cycles in the heap, and (b) to allow the heap size, classes and symbols to be known right after the image header.
"
Class {
	#name : #ASGenerator,
	#superclass : #Object,
	#instVars : [
		'fields',
		'elements',
		'header',
		'savedObject',
		'address',
		'nVars',
		'extra'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #accessing }
ASGenerator >> address [
	^ address
]

{ #category : #accessing }
ASGenerator >> address: anInteger nVars: anInteger2 extra: anInteger3 header: anObject [ 
	address := anInteger.
	nVars := anInteger2.
	extra := anInteger3.
	header := anObject.

]

{ #category : #initialization }
ASGenerator >> on: anASImage object: anObject [
	anASImage include: self object: anObject.
	savedObject := anObject.

]

{ #category : #process }
ASGenerator >> processOn: anASImage [
	1 to: nVars do: [: i |
		(savedObject instVarAt: i) astObject: anASImage
	]
]

{ #category : #writing }
ASGenerator >> writeFieldsOn: anASImage [
	header writeRawOn: anASImage.
	extra ifNotNil: [ extra writeRawOn: anASImage ].
	1 to: nVars do: [: i |
		((savedObject instVarAt: i) 	astImageObject: anASImage) writeRawOn: anASImage
	]
]
