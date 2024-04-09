"
I create a CompiledMethod in threaded form in Zig syntax.
"
Class {
	#name : 'ASZigOutput',
	#superclass : 'ASTextOutput',
	#instVars : [
		'ourObject',
		'alreadyGenerated',
		'delayedLabel',
		'forClass'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
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
ASZigOutput >> forClass [

	^ forClass 
]

{ #category : 'code generation' }
ASZigOutput >> generateCode [
	self subclassResponsibility 
]

{ #category : 'generation' }
ASZigOutput >> generateCode: anASNode [

	context for: anASNode generatorIs: self.
	ourObject := anASNode.
	alreadyGenerated := false.
	context processComponents.
	^ self
]

{ #category : 'generation' }
ASZigOutput >> generateCode: anASNode for: aClass [

	context for: anASNode generatorIs: self.
	ourObject := anASNode.
	forClass := aClass.
	alreadyGenerated := false.
	context processComponents
]

{ #category : 'accessing' }
ASZigOutput >> id: aString [ 
	mainStream nextPutAll: aString zigName 
]

{ #category : 'output' }
ASZigOutput >> label: result [

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
			comma; 
			lf ].
	^ result
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

{ #category : 'writing' }
ASZigOutput >> writeDefinitionOn: anObject [
self subclassResponsibility 
]

{ #category : 'writing' }
ASZigOutput >> writeInitializationOn: anObject [

	self subclassResponsibility
]
