"
I am a value that tracks literal BlockClosures
"
Class {
	#name : 'ASCBlockClosureValue',
	#superclass : 'ASCClassValue',
	#instVars : [
		'blocks',
		'scope',
		'asBlock',
		'codeGenerator',
		'parent'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'converting' }
ASCBlockClosureValue >> asBlock [

	^ asBlock
]

{ #category : 'converting' }
ASCBlockClosureValue >> asBlock: anASBlock [

	asBlock := anASBlock
]

{ #category : 'accessing' }
ASCBlockClosureValue >> blocks [
	^ blocks ifNil: [ blocks := OrderedCollection new ]
]

{ #category : 'API' }
ASCBlockClosureValue >> codeGenerator [

	^ codeGenerator ifNil: [ codeGenerator := ASCodeGeneratorBlock new parent: parent ast: asBlock ]
]

{ #category : 'accessing' }
ASCBlockClosureValue >> parent: anASCodeGenerator [ 
	parent := anASCodeGenerator
]

{ #category : 'accessing' }
ASCBlockClosureValue >> scope [
	^ scope
]

{ #category : 'accessing' }
ASCBlockClosureValue >> scope: aCollection [

	scope := aCollection
]
