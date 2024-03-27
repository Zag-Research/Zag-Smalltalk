Class {
	#name : #ASAllocationInfo,
	#superclass : #Object,
	#instVars : [
		'format',
		'instVars',
		'footerLength',
		'extra'
	],
	#classVars : [
		'AgeStatic',
		'HashCounter'
	],
	#pools : [
		'ASImage'
	],
	#category : 'ASTSmalltalk-Image'
}

{ #category : #'class initialization' }
ASAllocationInfo class >> initialize [

	AgeStatic := 7.
	self reset
]

{ #category : #'class initialization' }
ASAllocationInfo class >> reset [

	HashCounter := 0
]

{ #category : #accessing }
ASAllocationInfo >> extra: anObject [

	extra := anObject
]

{ #category : #accessing }
ASAllocationInfo >> footerLength: anObject [

	footerLength := anObject
]

{ #category : #formatting }
ASAllocationInfo >> format: fmt [ 

	format := fmt
]

{ #category : #accessing }
ASAllocationInfo >> header: classNumber [

	| length |
	length := instVars + extra + footerLength.
	length < MaxHeaderLength ifFalse: [ self error: 'header too big' ].
	HashCounter := HashCounter + 1.
	^ length << 4 + AgeStatic << 8 + format << 24 + HashCounter << 16 + classNumber
]

{ #category : #initialization }
ASAllocationInfo >> initialize [

	super initialize.
	instVars := 0.
	footerLength := 0.
	extra := 0
]

{ #category : #formatting }
ASAllocationInfo >> instVars: n [

	instVars := n
]

{ #category : #'as yet unclassified' }
ASAllocationInfo >> put: anObject into: anImage [

	| address class |
	address := anImage currentAddress.
	class := anImage class: anObject class.
	anImage putRawInt: (self header: class).
	1 to: instVars do: [ :n | anImage put: (anObject instVarAt: n) ].
	1 to: extra do: [ :n | anImage put: (anObject at: n) ].
	^ anImage pointer: address class: class
]
