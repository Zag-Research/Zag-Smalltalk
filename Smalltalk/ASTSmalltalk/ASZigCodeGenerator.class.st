"
I create a CompiledMethod in threaded form in Zig syntax.
"
Class {
	#name : #ASZigCodeGenerator,
	#superclass : #ASTextCodeGenerator,
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #writing }
ASZigCodeGenerator >> classListString [
		^ ''
]

{ #category : #accessing }
ASZigCodeGenerator >> comment: aString [

	self nextPutAll: ' // '.
	aString printOn: self
]

{ #category : #output }
ASZigCodeGenerator >> label: result [
	self
		nextPutAll: '":';
		nextPutAll: result;
		nextPut: $";
		comma.
	^ result
]

{ #category : #accessing }
ASZigCodeGenerator >> labelRef: aString [

	self
		nextPut: $";
		nextPutAll: aString;
		nextPut: $"
]

{ #category : #output }
ASZigCodeGenerator >> makeNextLabel [

	label := label + 1.
	^ String streamContents: [ :s |
		          s nextPut: $L.
		          label printOn: s ].

]

{ #category : #output }
ASZigCodeGenerator >> nextLabel [

	^ self label: self makeNextLabel.

]

{ #category : #output }
ASZigCodeGenerator >> putClass: aString [ 
self nextPutAll: 'c.';nextPutAll: aString;comma
]

{ #category : #output }
ASZigCodeGenerator >> putEmbedded: aString [

	self
		nextPutAll: '&e.';
		nextPutAll: aString;
		comma;lf
]

{ #category : #output }
ASZigCodeGenerator >> putPrimitive: primitiveNumber [

	self
		nextPutAll: '&p.p';
					print: primitiveNumber;
		comma;
		lf
]

{ #category : #output }
ASZigCodeGenerator >> putSymbol: aSymbol [

	(context predefinedSymbol: aSymbol)
		ifNotNil: [ :aString | mainStream << aString ]
		ifNil: [
			(symbols indexOf: aSymbol ifAbsent: [
				 symbols add: aSymbol.
				 context addSymbol: aSymbol.
				 symbols size ]) printOn: mainStream << 'Sym.i_' ]
]

{ #category : #initialization }
ASZigCodeGenerator >> refListString [
]

{ #category : #initialization }
ASZigCodeGenerator >> setLiteralString [

	^ String streamContents: [ :s | symbols do: [:each| s << (context printable: each) ] separatedBy: [s << ',']]
]
