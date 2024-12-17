"
I represent literal (hence immutable) values at compile time.

I have subclasses that include mutable versions
"
Class {
	#name : 'ASCLiteralValue',
	#superclass : 'ASCClassValue',
	#instVars : [
		'min'
	],
	#category : 'Zag-Core-CodeGen',
	#package : 'Zag-Core',
	#tag : 'CodeGen'
}

{ #category : 'common' }
ASCLiteralValue >> init: anObject [
" horrible hack "
	valueClass ifNil: [
		anObject class instanceSide == anObject ifTrue: [ valueClass := anObject ] ifFalse: [min := anObject.
valueClass := anObject class instanceSide].
		^ self ].
	self error
]

{ #category : 'accessing' }
ASCLiteralValue >> min [ 
^ min
]
