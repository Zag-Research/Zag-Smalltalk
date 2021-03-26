"
I write out ASTSmalltalk images
"
Class {
	#name : #ASImage,
	#superclass : #Object,
	#instVars : [
		'symbols',
		'classes',
		'baseAddress',
		'aStream'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #adding }
ASImage >> class: aString [
	self symbol: aString.
	^ classes at: aString asSymbol ifAbsentPut: [ classes size ].

]

{ #category : #initialization }
ASImage >> initialize [
	super initialize.
	baseAddress := 16r1000000000.
	symbols := Dictionary new.
	classes := Dictionary new.
	#(value value: value:value: value:value:value: value:value:value:value:
		) do: [: symbol |
			self symbol: symbol ].
	#(Object BlockClosure UndefinedObject True
		False SmallInteger Symbol Character
		Float String Class Metaclass Behavior System
		) do: [: symbol |
			self class: symbol ].

]

{ #category : #adding }
ASImage >> literal: l class: c [
	^ 16r7FF8000000000000 + (l bitShift: 3) + c
]

{ #category : #writing }
ASImage >> nextPutAll: aByteArray [
	^ aStream nextPutAll: aByteArray.
]

{ #category : #adding }
ASImage >> symbol: aString [ 
	^ self literal: (symbols at: aString asSymbol ifAbsentPut: [ symbols size ]) class: 6
]

{ #category : #writing }
ASImage >> writeClassTable [ 
	| table |
	table := Array new: classes size.
	classes keysAndValuesDo: [: key : value |
		table at: value+1 put: (symbols at: key) ].
	table do: [: sIndex | sIndex writeRaw32On: aStream ].

]

{ #category : #writing }
ASImage >> writeHeapOn [

]

{ #category : #writing }
ASImage >> writeImageOn: aWriteStream [
	aStream := aWriteStream.
	16r6567616d69545341 writeRawOn: aStream.
	self writeSizeOf: [self writeHeapOn ].
	self writeSizeOf: [self writeClassTable ].
	self writeSizeOf: [self writeSymbolTable ].
	
]

{ #category : #writing }
ASImage >> writeSizeOf: aBlock [
	| start |
	start := aStream position.
	aBlock value.
	[ (aStream position bitAnd: 7) = 0 ] whileFalse: [ aStream nextPut: 255 ].
	baseAddress + start writeRawOn: aStream.

]

{ #category : #writing }
ASImage >> writeSymbolTable [
	| table |
	table := Array new: symbols size.
	symbols keysAndValuesDo: [: key : value |
		table at: value+1 put: key ].
	table do: [: string | string writeWithCountOn: aStream ].

]
