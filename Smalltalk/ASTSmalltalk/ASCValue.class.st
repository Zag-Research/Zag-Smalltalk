"
I hold compile-time values that can be tracked and merged to support optimization.

This can be a literal value.

Subclasses represent local variables (and maybe more in the future).
"
Class {
	#name : 'ASCValue',
	#superclass : 'ASCLiteralValue',
	#instVars : [
		'max'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'accessing' }
ASCValue >> isRanged [

	^ max isNotNil
]

{ #category : 'accessing' }
ASCValue >> max [

	^ max
]

{ #category : 'accessing' }
ASCValue >> store: anObject [

	| aClass |
	aClass := anObject valueClass.
	valueClass ifNil: [
		valueClass := aClass.
		min := anObject min.
		max := anObject max.
		^ self ].
	valueClass = aClass ifTrue: [
		anObject isRanged ifFalse: [
			max := nil.
			^ self ].
		min <= anObject min
			ifFalse: [ min := anObject min ]
			ifTrue: [ anObject max <= max ifFalse: [ max := anObject max ] ].
		^ self ].
	valueClass := valueClass commonSuperclassWith: aClass.
	min := max := nil
]
