"
I create a CompiledMethod in threaded form in Zig syntax.
"
Class {
	#name : #ASZigOutput,
	#superclass : #ASTextOutput,
	#instVars : [
		'ourObject',
		'alreadyGenerated'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #writing }
ASZigOutput >> classListString [
		^ ''
]

{ #category : #accessing }
ASZigOutput >> comment: aString [

	self nextPutAll: ' // '.
	aString printOn: self
]

{ #category : #'code generation' }
ASZigOutput >> generateCode [
	self subclassResponsibility 
]

{ #category : #generation }
ASZigOutput >> generateCodeFor: anASNode [

	context for: anASNode generatorIs: self.
	ourObject := anASNode.
	alreadyGenerated := false.
	context processComponents
]

{ #category : #output }
ASZigOutput >> label: result [
	self
		nextPutAll: '":';
		nextPutAll: result;
		nextPut: $";
		comma.
	^ result
]

{ #category : #accessing }
ASZigOutput >> labelRef: aString [

	self
		nextPut: $";
		nextPutAll: aString;
		nextPut: $"
]

{ #category : #output }
ASZigOutput >> makeNextLabel [

	label := label + 1.
	^ String streamContents: [ :s |
		          s nextPut: $L.
		          label printOn: s ].

]

{ #category : #output }
ASZigOutput >> nextLabel [

	^ self label: self makeNextLabel.

]

{ #category : #output }
ASZigOutput >> putClass: aString [ 
self nextPutAll: 'c.';nextPutAll: aString;comma
]

{ #category : #API }
ASZigOutput >> putEmbedded: aString [

	self
		nextPutAll: '&e.';
		nextPutAll: aString;
		comma;lf
]

{ #category : #API }
ASZigOutput >> putInteger: anInteger [ 
	mainStream print: anInteger 
]

{ #category : #API }
ASZigOutput >> putPrimitive: primitiveNumber [

	self
		nextPutAll: '&p.p';
					print: primitiveNumber;
		comma;
		lf
]

{ #category : #output }
ASZigOutput >> putSymbol: aSymbol [

	(context predefinedSymbol: aSymbol)
		ifNotNil: [ :aString | mainStream << aString ]
		ifNil: [
			(symbols indexOf: aSymbol ifAbsent: [
				 symbols add: aSymbol.
				 context addSymbol: aSymbol.
				 symbols size ]) printOn: mainStream << 'Sym.i_' ]
]

{ #category : #initialization }
ASZigOutput >> refListString [
self flag: #daniel " what should this returnTop  ".
^''
]

{ #category : #accessing }
ASZigOutput >> selector [
self subclassResponsibility 
]

{ #category : #initialization }
ASZigOutput >> setLiteralString [

	^ String streamContents: [ :s |
		  symbols
			  do: [ :each | s nextPutAll: (context printable: each) ]
			  separatedBy: [ s nextPut: $, ] ]
]

{ #category : #writing }
ASZigOutput >> writeDefinitionOn: anObject [
self subclassResponsibility 
]

{ #category : #writing }
ASZigOutput >> writeInitializationOn: anObject [

	self subclassResponsibility
]
