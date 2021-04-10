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
		'currentAddress',
		'objects',
		'process',
		'output'
	],
	#classVars : [
		'BaseAddress',
		'MaxSize'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #export }
ASImage class >> exportMethods [
	^ self exportPackages flatCollect: [ :package |
			package methods
		]
]

{ #category : #export }
ASImage class >> exportPackages [
	^ RPackageOrganizer default packages select: [: package |
			((package name beginsWith: 'ASTSmalltalk')
					or: ((package name beginsWith: 'PC') 
								and: [(package name findString: '-Option') = 0]))
				and: [ (package name endsWith: '-Tests') not ]
		]
]

{ #category : #'class initialization' }
ASImage class >> initialize [
	MaxSize := 16r7"ff".
	BaseAddress := 16r1000000000.

]

{ #category : #adding }
ASImage >> class: aClass [
	| symbol |
	symbol := aClass name asSymbol.
	self symbol: symbol.
	^ classes at: symbol ifAbsentPut: [ classes size ].

]

{ #category : #objects }
ASImage >> generalObject: anObject [
	| gen |
	^ objects at: anObject ifAbsentPut: [
				gen := ASGenerator new on: self object: anObject.
				process add: gen.
				self processOut.
				gen
			]
]

{ #category : #adding }
ASImage >> header: header [
	| code key value |
	code := 0.
	header do: [: flag |
		flag isAssociation ifTrue: [
			key := flag key.
			value := flag value.
			key = #class ifTrue: [ code := code bitOr: ((self class: value) bitAnd: 16r1fffff) ].
			key = #hash ifTrue: [ code := code bitOr: (value bitAnd: 16r1fffff) << 21 ].
			key = #size ifTrue: [ code := code bitOr: (value bitAnd: MaxSize) << 48 ].
		] ifFalse: [
			flag = #hasInstVars ifTrue: [ code := code bitOr: 1 << 42 ].
			flag = #isVariable ifTrue: [ code := code bitOr: 1 << 43 ].
			flag = #isForwarded ifTrue: [ code := code bitOr: 1 << 63 ].
	]].
	^ code
]

{ #category : #objects }
ASImage >> imageObject: anObject [ 
	^ anObject astImageObject: self
]

{ #category : #adding }
ASImage >> include: anASGenerator object: anObject [
	| nVars extra size header |
	nVars := anObject class instSize.
	header := OrderedCollection with: #class->anObject class.
	header add: #hash -> currentAddress.
	anObject class isVariable ifTrue: [
		header add: #isVariable.
		extra := anObject size.
		nVars = 0 ifTrue: [
			extra <= MaxSize ifTrue: [
				header add: #size->extra.
				nVars := extra.
				extra := nil.
			] ifFalse: [
				nVars := extra.
			]
		] ifFalse: [
				header add: #size->nVars;add: #hasInstVars.
				nVars := nVars + extra.
		]
	]ifFalse: [
		header add: #size->nVars;add: #hasInstVars.
		extra := nil ].
	anASGenerator
		address: currentAddress
		nVars: nVars
		extra: extra
		header: (self header: header).
	size := 1 + (extra ifNil: [ 0 ] ifNotNil: [ 1 ]) + nVars.
	currentAddress := currentAddress + (size * 8)
]

{ #category : #initialization }
ASImage >> initialize [
	super initialize.
	baseAddress := BaseAddress.
	currentAddress := baseAddress + 16.
	objects := IdentityDictionary new.
	process := OrderedCollection new.
	output := OrderedCollection new.
	symbols := Dictionary new.
	classes := Dictionary new.
	#(value value: value:value: value:value:value: value:value:value:value:
		) do: [: symbol |
			self symbol: symbol ].
	{Object . BlockClosure . UndefinedObject . False .
		True . SmallInteger . Symbol . Character .
		Float . String . Class . Metaclass . Behavior . self class " this will be replaced with System "
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

{ #category : #objects }
ASImage >> object: aObject fields: nFields elements: nElements [ 
	self shouldBeImplemented 
]

{ #category : #accessing }
ASImage >> offset [
	^ currentAddress - baseAddress
]

{ #category : #process }
ASImage >> processOut [
	| gen |
	[ process notEmpty ] whileTrue: [
		gen := process removeFirst.
		output add: gen.
		gen 
	]
]

{ #category : #adding }
ASImage >> symbol: aString [ 
	^ self literal: (symbols at: aString asSymbol ifAbsentPut: [ symbols size ])+((aString select: [:c| c=$: ])size <<21) class: 6
]

{ #category : #writing }
ASImage >> writeClassTable [ 
	| table |
	classes size writeRawOn: aStream.
	table := Array new: classes size.
	classes keysAndValuesDo: [: key : value |
		table at: value+1 put: (self imageObject: key) ].
	table do: [: object | object writeRawOn: aStream ].

]

{ #category : #writing }
ASImage >> writeHeap [
	self offset / 8 writeRawOn: aStream.
	output do: [: gen | gen writeFieldsOn: self ].


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
