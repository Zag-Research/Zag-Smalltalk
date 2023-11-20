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
		'output',
		'newClasses',
		'selectors',
		'newSelectors',
		'methods'
	],
	#classVars : [
		'BaseAddress',
		'ClassMask',
		'FormatMask',
		'FormatShift',
		'HashMask',
		'HashShift',
		'SizeMask',
		'SizeShift'
	],
	#category : 'ASTSmalltalk-Image'
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
	BaseAddress := 16r1000000000.
	SizeMask := 16r7fff.
	SizeShift := 48.
	FormatMask := 16r1f.
	FormatShift := 40.
	HashShift := 20.
	HashMask := (1<<(FormatShift-HashShift))-1.
	ClassMask :=  (1<<HashShift)-1.


]

{ #category : #writing }
ASImage >> addFirstMethod: selector in: canonicalClasses [
	canonicalClasses do: [: class |
		class methodDict at: selector ifPresent: [: method | self method: method. ^ self ]
	]

]

{ #category : #accessing }
ASImage >> canonicalClasses: aClass [
	| name side |
	side := aClass isInstanceSide ifTrue: [ #instanceSide ] ifFalse: [ #classSide ].
	name := aClass instanceSide name.
	^ ((name beginsWith: 'AST') ifTrue: [
			Smalltalk at: (name allButFirst: 3) ifPresent: [: c | { aClass . c } ] ifAbsent: [ { aClass } ]
		] ifFalse: [
			Smalltalk at: ('AST',name) asSymbol ifPresent: [: c | { c . aClass } ] ifAbsent: [ { aClass } ]
		]) collect: side
]

{ #category : #adding }
ASImage >> class: aClass [
	| cc |
	cc := (self canonicalClasses: aClass) first.
	^ classes at: cc ifAbsentPut: [ newClasses add: cc. classes size ].

]

{ #category : #writing }
ASImage >> closeOverClassesAndSelectors [
	| tempClasses tempSelectors |
	[ newClasses size + newSelectors size > 0 ] whileTrue: [
		tempClasses := newClasses.
		newClasses := OrderedCollection new.
		tempSelectors := newSelectors.
		newSelectors := OrderedCollection new.
		tempClasses do: [: class | | cc |
			cc := self canonicalClasses: class.
			cc first superclass ifNotNil: [: superclass | self imageObject: superclass ].
			self imageObject: cc first.
			selectors do: [: selector |
				self addFirstMethod: selector in: cc.
			]
		].
		tempSelectors do: [: selector |
			classes keysDo: [: class |
				self addFirstMethod: selector in: (self canonicalClasses: class).
			]
		]
	]
]

{ #category : #compiling }
ASImage >> compileMethod: aMethod [
	self halt
]

{ #category : #writing }
ASImage >> createClassTable [ 
	| table |
	table := Array new: classes size.
	classes keysAndValuesDo: [: class : number |
		table at: number+1 put: (self imageObject: class) ].
	^ table
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
			key = #class ifTrue: [ code := code bitOr: ((self class: value) bitAnd: ClassMask) . key := nil ].
			key = #hash ifTrue: [ code := code bitOr: (value bitAnd: HashMask) << HashShift . key := nil ].
			key = #size ifTrue: [ code := code bitOr: (value bitAnd: SizeMask) << SizeShift . key := nil ].
			key = #format ifTrue: [ code := code bitOr: (value bitAnd: FormatMask) << FormatShift . key := nil ].
		] ifFalse: [
			key := flag.
			key = #isImmutable ifTrue: [ code := code bitOr: 1 << 47 . key := nil ].
			key = #isForwarded ifTrue: [ code := code bitOr: 1 << 63 . key := nil ].
		].
		key ifNotNil: [ self error: 'header field "',key asString,'" is illegal' ]
	].
	^ code
]

{ #category : #objects }
ASImage >> imageObject: anObject [ 
	^ anObject astImageObject: self
]

{ #category : #adding }
ASImage >> include: anASGenerator object: anObject [
	| nVars extra size header format fields |
	fields := anObject astFields.
	nVars := fields size.
	header := OrderedCollection with: #class -> anObject class.
	header add: #hash -> currentAddress.
	format := anObject astFormat.
	header add: #format -> format.
	extra := nil.
	(format & 24)>0 ifTrue: [ 
			(format & 17) = 1 ifTrue: [ 
					self error: 'indexable with instVars' ]
				ifFalse: [ 
					nVars <= SizeMask
						ifTrue: [ header add: #size -> nVars ]
						ifFalse: [
							extra := nVars.
							header add: #size -> SizeMask.
							self error: 'oversize array' ] ]
				  ]
		ifFalse: [ header add: #size -> nVars ].
	size := anASGenerator
		address: currentAddress
		extra: extra
		header: (self header: header)
		fields: fields.
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
	methods := Dictionary new.
	selectors := Set new.
	newClasses := OrderedCollection new.
	newSelectors := OrderedCollection new.
	#(valueWithArguments: cull: cull:cull: cull:cull:cull: cull:cull:cull:cull: " need to be first 5 symbols so that short-circuit on dispatch works "
		value value: value:value: value:value:value: value:value:value:value: " need to be the next 5 symbols "
		yourself == ~~ ~= = + - * size
		) do: [: symbol |
			self symbol: symbol ].
	{Object . BlockClosure . False . True .
		UndefinedObject . SmallInteger . Symbol . Character .
		Float . " all of those above must be in this order because they are the immediate/literal classes "
		ASTSystem
		} do: [: class |
			self class: class ].
	#(start) do: [: selector |
			self selector: selector ].
]

{ #category : #adding }
ASImage >> literal: l class: c [
	^ 16r7FF8000000000000 + (l bitShift: 3) + c
]

{ #category : #accessing }
ASImage >> method: aCompiledMethod [
	^ methods at: aCompiledMethod ifAbsentPut: [ self compileMethod: aCompiledMethod ]
]

{ #category : #accessing }
ASImage >> methods [
	^ methods
]

{ #category : #writing }
ASImage >> nextPutAll: aByteArray [
	^ aStream nextPutAll: aByteArray.
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
ASImage >> selector: aSymbol [
	^ selectors addIfNotPresent: aSymbol andDo:  [ newSelectors add: aSymbol ].

]

{ #category : #adding }
ASImage >> symbol: aString [
	aString isSymbol ifFalse: [ ^ self symbol: aString asSymbol ].
	^ self literal: (symbols at: aString ifAbsentPut: [ symbols size + 1])+(aString numArgs <<25) class: 6
]

{ #category : #writing }
ASImage >> writeHeap [
	self offset / 8 writeRawOn: aStream.
	output do: [: gen | gen writeFieldsOn: self ].


]

{ #category : #writing }
ASImage >> writeImageOn: aWriteStream [
	| table |
	aStream := aWriteStream.
	16r6567616d69545341 writeRawOn: aStream.
	self closeOverClassesAndSelectors.
	table := self createClassTable.
	self writeHeap.
	table writeRawOn: aStream.
	self writeSymbolTable.
	
]

{ #category : #writing }
ASImage >> writeSymbolTable [
	| table |
	symbols size writeRawOn: aStream.
	table := Array new: symbols size.
	symbols keysAndValuesDo: [: key : value |
		table at: value put: key ].
	table do: [: string | string writeWithCountOn: aStream ].

]
