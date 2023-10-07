"
I represent literal (hence immutable) values at compile time.

I have subclasses that include mutable versions
"
Class {
	#name : #ASCLiteralValue,
	#superclass : #Object,
	#instVars : [
		'valueClass',
		'min'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #accessing }
ASCLiteralValue >> init: anObject [

	valueClass ifNil: [
		min := anObject.
		valueClass := anObject class.
		^ self ].
	self error
]

{ #category : #testing }
ASCLiteralValue >> isMutable [

	^ false
]

{ #category : #accessing }
ASCLiteralValue >> isRanged [
^
valueClass canUnderstand: #'<='
]

{ #category : #accessing }
ASCLiteralValue >> kind [

	^ valueClass
]

{ #category : #accessing }
ASCLiteralValue >> max [

	^ self isRanged ifTrue: [ min ] ifFalse: [ nil ]
]

{ #category : #accessing }
ASCLiteralValue >> min [ 
^ min
]

{ #category : #accessing }
ASCLiteralValue >> valueClass [
	^ valueClass
]
