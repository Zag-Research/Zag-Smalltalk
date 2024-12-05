"
I represent encoders for image export.

I have the class-side methods to create and load a ZagConstantsXXXXX class that for my subclasses.
"
Class {
	#name : 'ZagImageEncoder',
	#superclass : 'Object',
	#classInstVars : [
		'constants'
	],
	#category : 'Zag-Exporter-Encoding',
	#package : 'Zag-Exporter',
	#tag : 'Encoding'
}

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

{ #category : 'initialization' }
ZagImageEncoder class >> constants [

	^ constants
]

{ #category : 'initialization' }
ZagImageEncoder class >> constants: aClass [

	constants := aClass
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
	encoder constants: class.
	constants := class.
	^ class
]

{ #category : 'class initialization' }
ZagImageEncoder class >> constantsClassFrom: stream [

	^ self constantsClass: 'ZagConstants' from: stream
]

{ #category : 'converting' }
ZagImageEncoder class >> convertToZag: anObject [

	^ anObject convertToZag: self
]

{ #category : 'converting' }
ZagImageEncoder class >> double: aNumber for: anImage [

	^ self subclassResponsibility
]

{ #category : 'class initialization' }
ZagImageEncoder class >> from: stream [
	"self constantsClass: #ZagConstants from: (ReadStream on: 'Zag
	BaseAddress := 16r1000000000.
	SizeMask := 16r7fff.
')"

	| type class superClass |
	stream reset.
	type := stream nextLine.
	superClass := type = 'Nan'
		              ifTrue: [ ZagImageEncodeNan ]
		              ifFalse: [
			              type = 'Zag' ifFalse: [
				              self error: 'Unrecognized coding type: ' , type ].
			              ZagImageEncodeZag ].
	class := (superClass << ('ZagConstants' , self hash asString)) build.
	class constantMethodsFromStream: stream.
	ZagImageEncoder constants: class.
	constants := class.
	^ class
]

{ #category : 'class initialization' }
ZagImageEncoder class >> handleComments: stream [

	[
	stream skipSeparators.
	stream atEnd not and: [ stream peek = $" ] ] whileTrue: [
		stream next.
		[ stream atEnd or: [ stream next = $" ] ] whileFalse ]
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

{ #category : 'converting' }
ZagImageEncoder class >> pointer: p [

	^ self subclassResponsibility
]

{ #category : 'converting' }
ZagImageEncoder class >> smallInteger: aNumber [

	^ self subclassResponsibility
]
