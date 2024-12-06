"
I represent encoders for image export.

I have the class-side methods to create and load a ZagConstantsXXXXX class that for my subclasses.
"
Class {
	#name : 'ZagImageEncoder',
	#superclass : 'Object',
	#classInstVars : [
		'zagConstants',
		'nanConstants'
	],
	#category : 'Zag-Exporter-Encoding',
	#package : 'Zag-Exporter',
	#tag : 'Encoding'
}

{ #category : 'private - temporary' }
ZagImageEncoder class >> ageStatic [

	^ 7
]

{ #category : 'private - temporary' }
ZagImageEncoder class >> baseAddress [

	^ 16r100000
]

{ #category : 'as yet unclassified' }
ZagImageEncoder class >> boolean: value [ 
	^ self subclassResponsibility
]

{ #category : 'private - initialization' }
ZagImageEncoder class >> constantMethodsFromStream: stream [

	| constantName constant |
	[ stream atEnd ] whileFalse: [
		self handleComments: stream.
		constantName := stream upTo: $ .
		self handleComments: stream.
		stream peek = $: ifTrue: [ stream next ].
		stream peek = $= ifTrue: [ stream next ].
		self handleComments: stream.
		constant := stream nextLine.
		constant last = $. ifTrue: [ constant := constant allButLast ].
		self class
			compile: constantName , ' ^' , constant
			classified: 'Zag constant'
			notifying: nil ]
]

{ #category : 'class initialization' }
ZagImageEncoder class >> constantsClass: className from: stream [

	| type class encoder |
	self handleComments: stream.
	type := stream nextLine.
	encoder := type = 'Nan'
		           ifTrue: [ ZagImageEncodeNan ]
		           ifFalse: [
			           type = 'Zag' ifFalse: [
				           self error: 'Unrecognized coding type: ' , type ].
			           ZagImageEncodeZag ].
	class := (encoder << (className , self hash asString)) build.
	class constantMethodsFromStream: stream.
	^ class saveConstants
]

{ #category : 'converting' }
ZagImageEncoder class >> convertToZag: anObject [

	^ anObject convertToZag: self
]

{ #category : 'private - temporary' }
ZagImageEncoder class >> directIndexed [

	^ 115
]

{ #category : 'converting' }
ZagImageEncoder class >> double: aNumber for: anImage [

	^ self subclassResponsibility
]

{ #category : 'class initialization' }
ZagImageEncoder class >> handleComments: stream [

	[
	stream skipSeparators.
	stream atEnd not and: [ stream peek = $" ] ] whileTrue: [
		stream next.
		[ stream atEnd or: [ stream next = $" ] ] whileFalse ]
]

{ #category : 'private - temporary' }
ZagImageEncoder class >> initialize [

	ZagImageEncodeNan saveConstants.
	ZagImageEncodeZag saveConstants
]

{ #category : 'initialization' }
ZagImageEncoder class >> initializeFromStdin [

	| stdin |
	stdin := CommandLineUIManager new stdin.
]

{ #category : 'converting' }
ZagImageEncoder class >> literal: l class: c [

	^ self subclassResponsibility
]

{ #category : 'private - temporary' }
ZagImageEncoder class >> maxHeaderLength [

	^ 4093
]

{ #category : 'initialization' }
ZagImageEncoder class >> nanConstants: constantsClass [

	^ nanConstants := constantsClass
]

{ #category : 'private - temporary' }
ZagImageEncoder class >> notIndexable [

	^ 112
]

{ #category : 'converting' }
ZagImageEncoder class >> pointer: p [

	^ self subclassResponsibility
]

{ #category : 'class initialization' }
ZagImageEncoder class >> saveConstants [

	^ self subclassResponsibility
]

{ #category : 'converting' }
ZagImageEncoder class >> smallInteger: aNumber [

	^ self subclassResponsibility
]

{ #category : 'as yet unclassified' }
ZagImageEncoder class >> undefinedObject [
	^ self subclassResponsibility
]

{ #category : 'initialization' }
ZagImageEncoder class >> zagConstants: constantsClass [

	^ zagConstants := constantsClass
]
