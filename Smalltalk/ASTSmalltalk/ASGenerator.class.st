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
		'header',
		'address',
		'extra'
	],
	#category : 'ASTSmalltalk-Image'
}

{ #category : #accessing }
ASGenerator >> address [
	^ address
]

{ #category : #accessing }
ASGenerator >> address: anInteger extra: anInteger3 header: anObject fields: anObject1 [ 
	address := anInteger.
	extra := anInteger3.
	header := anObject.
	fields := anObject1.
	^ 1 + (extra ifNil: [ 0 ] ifNotNil: [ 1 ]) + fields size
]

{ #category : #initialization }
ASGenerator >> on: anASImage object: anObject [
	anASImage include: self object: anObject
]

{ #category : #process }
ASGenerator >> processOn: anASImage [
	fields do: [: value |
		value astObject: anASImage
	]
]

{ #category : #writing }
ASGenerator >> writeFieldsOn: anASImage [
	header writeRawOn: anASImage.
	extra ifNotNil: [ extra writeRawOn: anASImage ].
	fields do: [: value |
		(value astImageObject: anASImage) writeRawOn: anASImage
	].

]
