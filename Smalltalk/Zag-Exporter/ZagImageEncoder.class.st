"
I am an abstract class to show all the methods encoders need.
"
Class {
	#name : 'ZagImageEncoder',
	#superclass : 'Object',
	#pools : [
		'ZagConstants'
	],
	#category : 'Zag-Exporter-Encoding',
	#package : 'Zag-Exporter',
	#tag : 'Encoding'
}

{ #category : 'converting' }
ZagImageEncoder class >> boolean: value [ 
	^ self subclassResponsibility
]

{ #category : 'converting' }
ZagImageEncoder class >> convertToZag: anObject [

	^ anObject convertToZag: self
]

{ #category : 'converting' }
ZagImageEncoder class >> double: aNumber for: anImage [

	^ self subclassResponsibility
]

{ #category : 'converting' }
ZagImageEncoder class >> literal: l class: c [

	^ self subclassResponsibility
]

{ #category : 'converting' }
ZagImageEncoder class >> pointer: p [

	^ self subclassResponsibility
]

{ #category : 'converting' }
ZagImageEncoder class >> smallInteger: aNumber [

	^ self subclassResponsibility
]

{ #category : 'converting' }
ZagImageEncoder class >> undefinedObject [
	^ self subclassResponsibility
]
