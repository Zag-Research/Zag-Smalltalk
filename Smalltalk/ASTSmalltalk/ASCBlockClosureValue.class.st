"
I am a value that tracks literal BlockClosures
"
Class {
	#name : 'ASCBlockClosureValue',
	#superclass : 'ASCClassValue',
	#instVars : [
		'blocks',
		'scope'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'accessing' }
ASCBlockClosureValue >> blocks [
	^ blocks ifNil: [ blocks := OrderedCollection new ]
]

{ #category : 'accessing' }
ASCBlockClosureValue >> scope [
	^ scope
]

{ #category : 'accessing' }
ASCBlockClosureValue >> scope: aCollection [

	scope := aCollection
]
