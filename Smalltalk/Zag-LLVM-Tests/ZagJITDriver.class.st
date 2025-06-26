"
I feed a (LLVM) genarator with a sequence of calls corresponding to a threaded sequence.

I emulate the zig code that is used to generate an LLVM-jitted native-code method from an existing threaded method.

That is, I send messages to the generator for each of the threaded codes in the threaded code for the method.
I also implement several helper functions required by such a generator.
"
Class {
	#name : 'ZagJITDriver',
	#superclass : 'ZagOutputSourceAPI',
	#instVars : [
		'index',
		'array',
		'address',
		'dispatcher'
	],
	#classVars : [
		'HeaderSize'
	],
	#category : 'Zag-LLVM-Tests-Support',
	#package : 'Zag-LLVM-Tests',
	#tag : 'Support'
}

{ #category : 'class initialization' }
ZagJITDriver class >> initialize [

	HeaderSize := 40
]

{ #category : 'private' }
ZagJITDriver >> addressOf: offset [

	^ address + HeaderSize + (offset - 1 * 8)
]

{ #category : 'private' }
ZagJITDriver >> addressOfLabel: anInteger [

	| target offset element |
	offset := anInteger.
	target := index.
	self halt.
	[
	(element := array at: target) isArray not or: [
		element first ~= #label or: [
			offset := offset - offset sign.
			offset ~= 0 ] ] ] whileTrue: [ target := target + offset sign ].
	^ self addressOf: target
]

{ #category : 'API' }
ZagJITDriver >> codeGen: anObject [

	dispatcher := anObject.
	anObject source: self
]

{ #category : 'API' }
ZagJITDriver >> dispatcher: anObject [

	dispatcher := anObject.
	anObject driver: self
]

{ #category : 'API' }
ZagJITDriver >> genFrom: collection [

	index := 1.
	array := collection.
	^ self iterate
]

{ #category : 'private' }
ZagJITDriver >> iterate [

	| selector |
	index > array size ifTrue: [ ^ nil ].
	selector := self valueOf.
	index := index + 1.
	dispatcher perform: selector.
	^ self iterate
]

{ #category : 'API' }
ZagJITDriver >> methodAddress: anObject [

	address := anObject
]

{ #category : 'API' }
ZagJITDriver >> nextAddress [
" returns the next word as a Code address "

	| object |
	object := self valueOf.
	index := index + 1.
	^ object
]

{ #category : 'API' }
ZagJITDriver >> nextObject [

	| object |
	object := self valueOf.
	index := index + 1.
	^ object
]

{ #category : 'API' }
ZagJITDriver >> thatAddress [

	^ self addressOf: index - 1
]

{ #category : 'private' }
ZagJITDriver >> valueOf [

	| object |
	object := array at: index.
	object isArray ifFalse: [ ^ object ].
	object first = #method ifTrue: [ ^ address ].
	object first = #label ifTrue: [ ^ #label ].
	object first isInteger ifTrue: [ ^ self addressOfLabel: object first ].
	^ self error: object
]
