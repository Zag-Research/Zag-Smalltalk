"
I create a CompiledMethod in threaded form in Zig syntax.
"
Class {
	#name : 'ASZigOutput',
	#superclass : 'ASTextOutput',
	#instVars : [
		'delayedLabel'
	],
	#category : 'ASTSmalltalk-Output',
	#package : 'ASTSmalltalk',
	#tag : 'Output'
}

{ #category : 'writing' }
ASZigOutput >> classListString [
		^ ''
]

{ #category : 'accessing' }
ASZigOutput >> comment: aString [

	self nextPutAll: ' // '.
	aString printOn: self
]

{ #category : 'accessing' }
ASZigOutput >> id: aString [ 
	mainStream nextPutAll: aString zigName 
]

{ #category : 'accessing' }
ASZigOutput >> labelRef: aString [

	self
		nextPut: $";
		nextPutAll: aString;
		nextPut: $"
]

{ #category : 'output' }
ASZigOutput >> makeNextLabel [

	label := label + 1.
	^ String streamContents: [ :s |
		          s nextPut: $L.
		          label printOn: s ].

]

{ #category : 'output' }
ASZigOutput >> nextLabel [

	^ self label: self makeNextLabel.

]

{ #category : 'accessing' }
ASZigOutput >> ourObject [

	^ ourObject 
]

{ #category : 'output' }
ASZigOutput >> outputMethodDefinition: bbLabel methods: methods [
]

{ #category : 'output' }
ASZigOutput >> putClass: aString [ 
self nextPutAll: 'c.';nextPutAll: aString;comma
]

{ #category : 'output' }
ASZigOutput >> putDirectSymbol: aSymbol [

	(context predefinedSymbol: aSymbol)
		ifNotNil: [ :aString | mainStream << aString ]
		ifNil: [
				context addSymbol: aSymbol.
			mainStream << 'sym.' << aSymbol zigName ]
]

{ #category : 'API' }
ASZigOutput >> putEmbedded: aString [

	self
		nextPutAll: '&e.';
		nextPutAll: aString;
		comma
]

{ #category : 'API' }
ASZigOutput >> putInteger: anInteger [ 
	mainStream print: anInteger 
]

{ #category : 'API' }
ASZigOutput >> putPrimitive: primitiveNumber [

	self
		nextPutAll: '&p.p';
					print: primitiveNumber;
		comma
]

{ #category : 'output' }
ASZigOutput >> putSymbol: aSymbol [

	(context predefinedSymbol: aSymbol)
		ifNotNil: [ :aString | mainStream << aString ]
		ifNil: [
			(symbols indexOf: aSymbol ifAbsent: [
				 symbols add: aSymbol.
				 context addSymbol: aSymbol.
				 symbols size ]) printOn: mainStream << 'Sym.i_' ]
]

{ #category : 'initialization' }
ASZigOutput >> refListString [
self flag: #daniel " what should this returnTop  ".
^''
]

{ #category : 'initialization' }
ASZigOutput >> setLiteralString [

	^ String streamContents: [ :s |
		  symbols
			  do: [ :each | s nextPutAll: (context printable: each) ]
			  separatedBy: [ s nextPut: $, ] ]
]
