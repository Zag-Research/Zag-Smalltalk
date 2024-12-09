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

{ #category : 'private - temporary - header' }
ZagImageEncoder class >> ageStatic [

	^ 7
]

{ #category : 'private - temporary' }
ZagImageEncoder class >> baseAddress [

	^ 16r100000
]

{ #category : 'converting' }
ZagImageEncoder class >> boolean: value [ 
	^ self subclassResponsibility
]

{ #category : 'private - temporary - class' }
ZagImageEncoder class >> classCharacter [

	^ 14
]

{ #category : 'private - temporary - class' }
ZagImageEncoder class >> classFalse [

	^ 10
]

{ #category : 'private - temporary - class' }
ZagImageEncoder class >> classSmallInteger [

	^ 12
]

{ #category : 'private - temporary - class' }
ZagImageEncoder class >> classString [

	^ 31
]

{ #category : 'private - temporary - class' }
ZagImageEncoder class >> classSymbol [

	^ 13
]

{ #category : 'private - temporary - class' }
ZagImageEncoder class >> classUndefinedObject [

	^ 23
]

{ #category : 'private - temporary' }
ZagImageEncoder class >> comment [

	^ ''
]

{ #category : 'private - class initialization' }
ZagImageEncoder class >> constantMethodsFromStream: stream [

	| constantName constant |
	[ stream atEnd ] whileFalse: [
		self handleComments: stream.
		constantName := String streamContents: [ :s |
			                [ stream peek asCharacter isAlphaNumeric ]
				                whileTrue: [ s nextPut: stream next asCharacter ] ].
		self handleComments: stream.
		stream peek asCharacter = $: ifTrue: [ stream next ].
		stream peek asCharacter = $= ifTrue: [ stream next ].
		self handleComments: stream.
		constant := self nextLine: stream.
		constant last = $. ifTrue: [ constant := constant allButLast ].
		self class
			compile: constantName , ' ^' , constant
			classified: 'Zag constant'
			notifying: nil ]
]

{ #category : 'private - class initialization' }
ZagImageEncoder class >> constantsClass: className from: stream [

	| type class encoder |
	self handleComments: stream.
	type := self nextLine: stream.
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

{ #category : 'converting' }
ZagImageEncoder class >> double: aNumber for: anImage [

	^ self subclassResponsibility
]

{ #category : 'private - temporary - header' }
ZagImageEncoder class >> formatDirectIndexed [

	^ 115
]

{ #category : 'private - temporary - header' }
ZagImageEncoder class >> formatNotIndexable [

	^ 112
]

{ #category : 'private - class initialization' }
ZagImageEncoder class >> handleComments: stream [

	[
	[ stream atEnd not and: [ stream peek asCharacter isSeparator ] ] whileTrue: [
		stream next ].
	stream atEnd not and: [ stream peek asCharacter = $" ] ] whileTrue: [
		stream next.
		[ stream atEnd or: [ stream next asCharacter = $" ] ] whileFalse ].
	[ stream atEnd not and: [ stream peek asCharacter isSeparator ] ] whileTrue: [
		stream next ]
]

{ #category : 'class initialization' }
ZagImageEncoder class >> initialize [

	ZagImageEncodeNan saveConstants.
	ZagImageEncodeZag saveConstants
]

{ #category : 'class initialization' }
ZagImageEncoder class >> initializeFromStdin [

	| stdin constantsClass saveImage |
	stdin := CommandLineUIManager new stdin.
	constantsClass := self constantsClass: #ZagConstants from: stdin.
	saveImage := constantsClass validInitialization
		             ifTrue: [
			             ZagImageEncoderQuestion
				             choiceWithPrompt: 'Valid Constant Initialization'
				             text:
					             'I am going to exit this image.
'
					             , constantsClass comment
					             , '
Do you want me to save the image or discard it?
Note: if you have this image open elsewhere you should choose "No".' ]
		             ifFalse: [ false ].
	saveImage ifNotNil: [ Smalltalk snapshot: saveImage andQuit: true ]
]

{ #category : 'converting' }
ZagImageEncoder class >> literal: l class: c [

	^ self subclassResponsibility
]

{ #category : 'private - temporary - header' }
ZagImageEncoder class >> maxHeaderLength [

	^ 4093
]

{ #category : 'private - class initialization' }
ZagImageEncoder class >> nanConstants: constantsClass [

	^ nanConstants := constantsClass
]

{ #category : 'private - class initialization' }
ZagImageEncoder class >> nextLine: stream [

	stream atEnd ifTrue: [ ^ nil ].
	^ String streamContents: [ :out |
		  | eol char |
		  eol := false.
		  [ eol ] whileFalse: [
			  char := stream next.
			  (char isNil or: [ char asInteger == 10 ])
				  ifTrue: [ eol := true ]
				  ifFalse: [
					  char asInteger == 13
						  ifTrue: [
							  eol := true.
							  stream peek asInteger = 10 ifTrue: [ stream next ] ]
						  ifFalse: [ out nextPut: char asCharacter] ] ] ]
]

{ #category : 'converting' }
ZagImageEncoder class >> pointer: p [

	^ self subclassResponsibility
]

{ #category : 'private - class initialization' }
ZagImageEncoder class >> saveConstants [

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

{ #category : 'private - temporary' }
ZagImageEncoder class >> validInitialization [

	^ false
]

{ #category : 'private - class initialization' }
ZagImageEncoder class >> zagConstants: constantsClass [

	^ zagConstants := constantsClass
]
