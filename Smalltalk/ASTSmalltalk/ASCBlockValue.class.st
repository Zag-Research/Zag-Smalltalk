"
I am a value that tracks literal BlockClosures
"
Class {
	#name : #ASCBlockValue,
	#superclass : #ASCClassValue,
	#instVars : [
		'blocks'
	],
	#category : 'ASTSmalltalk-CodeGen'
}

{ #category : #accessing }
ASCBlockValue >> blocks [
	^ blocks ifNil: [ blocks := OrderedCollection new ]
]
