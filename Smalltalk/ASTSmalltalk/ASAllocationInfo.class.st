Class {
	#name : 'ASAllocationInfo',
	#superclass : 'Object',
	#instVars : [
		'footerLength',
		'extra',
		'instVars',
		'format'
	],
	#classVars : [
		'AgeStatic',
		'HashCounter'
	],
	#pools : [
		'ASImage'
	],
	#category : 'ASTSmalltalk-Image',
	#package : 'ASTSmalltalk',
	#tag : 'Image'
}

{ #category : 'initialization' }
ASAllocationInfo class >> initialize [

	AgeStatic := 7.
	self reset
]

{ #category : 'initialization' }
ASAllocationInfo class >> reset [

	HashCounter := 0
]

{ #category : 'accessing' }
ASAllocationInfo >> extra: anObject [

	extra := anObject
]

{ #category : 'accessing' }
ASAllocationInfo >> footerLength: anObject [

	footerLength := anObject
]

{ #category : 'accessing' }
ASAllocationInfo >> format: fmt [ 

	format := fmt
]

{ #category : 'accessing' }
ASAllocationInfo >> header: classNumber [

	| length |
	length := instVars + extra + footerLength.
	length < MaxHeaderLength ifFalse: [ self error: 'header too big' ].
	HashCounter := HashCounter + 1.
	^ length << 4 + AgeStatic << 8 + format << 24 + HashCounter << 16
	  + classNumber
]

{ #category : 'initialization' }
ASAllocationInfo >> initialize [

	super initialize.
	instVars := 0.
	footerLength := 0.
	extra := 0
]

{ #category : 'accessing' }
ASAllocationInfo >> instVars: n [

	instVars := n
]

{ #category : 'accessing' }
ASAllocationInfo >> put: anObject into: anImage [

	| address class |
	class := anImage class: anObject class.
	address := anImage putRawInt: (self header: class).
	1 to: instVars do: [ :n | (anObject instVarAt: n) astImageObject: anImage ].
	1 to: extra do: [ :n | (anObject at: n) astImageObject: anImage ].
	^ anImage pointer: address class: class
]
