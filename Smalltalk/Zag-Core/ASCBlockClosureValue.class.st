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
	#category : 'Zag-Core-CodeGen',
	#package : 'Zag-Core',
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

	^ codeGenerator ifNil: [
		  codeGenerator := ASCodeGeneratorBlock new
			                   parent: parent
									source: self
			                   ast: asBlock ]
]

{ #category : 'accessing' }
ASCBlockClosureValue >> codeGenerator: anASCodeGenerator [ 
	codeGenerator := anASCodeGenerator
]

{ #category : 'data flow instructions' }
ASCBlockClosureValue >> finalAnalysis [
	self codeGenerator finalAnalysis
]

{ #category : 'flow' }
ASCBlockClosureValue >> flowClass [

	^ BlockClosure
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
