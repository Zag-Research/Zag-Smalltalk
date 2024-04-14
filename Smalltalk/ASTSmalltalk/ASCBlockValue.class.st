"
I am a value that tracks literal BlockClosures
"
Class {
	#name : 'ASCBlockValue',
	#superclass : 'ASCClassValue',
	#instVars : [
		'blocks'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'accessing' }
ASCBlockValue >> blocks [
	^ blocks ifNil: [ blocks := OrderedCollection new ]
]
