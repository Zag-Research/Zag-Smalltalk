"
I represent literal (hence immutable) values at compile time.

I have subclasses that include mutable versions
"
Class {
	#name : #ASCLiteralValue,
	#superclass : #ASCClassValue,
	#instVars : [
		'min'
	],
	#category : 'ASTSmalltalk-CodeGen'
}

{ #category : #common }
ASCLiteralValue >> init: anObject [

	valueClass ifNil: [
		min := anObject.
		valueClass := anObject class.
		^ self ].
	self error 
]

{ #category : #accessing }
ASCLiteralValue >> min [ 
^ min
]
