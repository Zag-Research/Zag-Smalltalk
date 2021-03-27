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
		'aStream',
		'currentAddress'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #adding }
ASImage >> class: aClass [
	| symbol |
	symbol := aClass name asSymbol.
	self symbol: symbol.
	^ classes at: symbol ifAbsentPut: [ classes size ].

]

{ #category : #initialization }
ASImage >> initialize [
	super initialize.
	currentAddress := baseAddress := 16r1000000000.
	symbols := Dictionary new.
	classes := Dictionary new.
	#(value value: value:value: value:value:value: value:value:value:value:
		) do: [: symbol |
			self symbol: symbol ].
	{Object . BlockClosure . UndefinedObject . False .
		True . SmallInteger . Symbol . Character .
		Float . String . Class . Metaclass . Behavior . ASImage
		} do: [: symbol |
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

{ #category : #objects }
ASImage >> object: anObject [ 
	^ anObject astObject: self
]

{ #category : #accessing }
ASImage >> offset [
	^ currentAddress - baseAddress
]

{ #category : #adding }
ASImage >> symbol: aString [ 
	^ self literal: (symbols at: aString asSymbol ifAbsentPut: [ symbols size ])+((aString select: [:c| c=$: ])size <<25) class: 6
]

{ #category : #writing }
ASImage >> writeClassTable [ 
	| table |
	classes size writeRawOn: aStream.
	table := Array new: classes size.
	classes keysAndValuesDo: [: key : value |
		table at: value+1 put: (self object: key) ].
	table do: [: object | object writeRawOn: aStream ].

]

{ #category : #writing }
ASImage >> writeHeap [
	self offset / 8 writeRawOn: aStream.

]

{ #category : #writing }
ASImage >> writeImageOn: aWriteStream [
	aStream := aWriteStream.
	16r6567616d69545341 writeRawOn: aStream.
	self writeHeap.
	self writeClassTable.
	self writeSymbolTable.
	
]

{ #category : #writing }
ASImage >> writeSymbolTable [
	| table |
	symbols size writeRawOn: aStream.
	table := Array new: symbols size.
	symbols keysAndValuesDo: [: key : value |
		table at: value+1 put: key ].
	table do: [: string | string writeWithCountOn: aStream ].

]
