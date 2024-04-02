"
I create a CompiledMethod in threaded form in Zig syntax.
"
Class {
	#name : 'ASLlvmTextOutput',
	#superclass : 'ASTextOutput',
	#classTraits : '{} + TraitedClass',
	#instVars : [
		'delayedLabel'
	],
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'writing' }
ASLlvmTextOutput >> classListString [
	^ ''
]

{ #category : 'accessing' }
ASLlvmTextOutput >> comment: aString [

	self nextPutAll: ' // '.
	aString printOn: self
]

{ #category : 'accessing' }
ASLlvmTextOutput >> id: aString [ 
	textStream nextPutAll: aString zigName 
]

{ #category : 'output' }
ASLlvmTextOutput >> label: result [

	delayedLabel = result ifFalse: [
		delayedLabel ifNotNil: [
			self
				putEmbedded: #br;
				labelRef: delayedLabel] ].
			delayedLabel := nil .
	result ifNotNil: [
		self
			nextPutAll: '":';
			nextPutAll: result;
			nextPut: $";
			comma ].
	^ result
]

{ #category : 'accessing' }
ASLlvmTextOutput >> labelRef: aString [

	self
		nextPut: $";
		nextPutAll: aString;
		nextPut: $"
]

{ #category : 'output' }
ASLlvmTextOutput >> makeNextLabel [

	label := label + 1.
	^ String streamContents: [ :s |
		          s nextPut: $L.
		          label printOn: s ].

]

{ #category : 'output' }
ASLlvmTextOutput >> nextLabel [

	^ self label: self makeNextLabel.

]

{ #category : 'accessing' }
ASLlvmTextOutput >> ourObject [

	^ ourObject 
]

{ #category : 'output' }
ASLlvmTextOutput >> putClass: aString [ 
self nextPutAll: 'c.';nextPutAll: aString;comma
]

{ #category : 'output' }
ASLlvmTextOutput >> putDirectSymbol: aSymbol [

	(context predefinedSymbol: aSymbol)
		ifNotNil: [ :aString | textStream << aString ]
		ifNil: [
				context addSymbol: aSymbol.
			textStream << 'sym.' << aSymbol zigName ]
]

{ #category : 'API' }
ASLlvmTextOutput >> putEmbedded: aString [

	self
		nextPutAll: '&e.';
		nextPutAll: aString;
		comma;lf
]

{ #category : 'API' }
ASLlvmTextOutput >> putInteger: anInteger [ 
	textStream print: anInteger 
]

{ #category : 'API' }
ASLlvmTextOutput >> putPrimitive: primitiveNumber [

	self
		nextPutAll: '&p.p';
					print: primitiveNumber;
		comma;
		lf
]

{ #category : 'output' }
ASLlvmTextOutput >> putSymbol: aSymbol [

	(context predefinedSymbol: aSymbol)
		ifNotNil: [ :aString | textStream << aString ]
		ifNil: [
			(symbols indexOf: aSymbol ifAbsent: [
				 symbols add: aSymbol.
				 context addSymbol: aSymbol.
				 symbols size ]) printOn: textStream << 'Sym.i_' ]
]

{ #category : 'initialization' }
ASLlvmTextOutput >> refListString [
self flag: #daniel " what should this returnTop  ".
^''
]

{ #category : 'initialization' }
ASLlvmTextOutput >> setLiteralString [

	^ String streamContents: [ :s |
		  symbols
			  do: [ :each | s nextPutAll: (context printable: each) ]
			  separatedBy: [ s nextPut: $, ] ]
]

{ #category : 'writing' }
ASLlvmTextOutput >> writeDefinitionOn: anObject [
self subclassResponsibility 
]

{ #category : 'writing' }
ASLlvmTextOutput >> writeInitializationOn: anObject [

	self subclassResponsibility
]
