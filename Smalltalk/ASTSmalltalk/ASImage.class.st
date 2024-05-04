"
I write out ASTSmalltalk images
"
Class {
	#name : 'ASImage',
	#superclass : 'SharedPool',
	#instVars : [
		'symbols',
		'classes',
		'aStream',
		'currentAddress',
		'objects',
		'process',
		'newClasses',
		'selectors',
		'newSelectors',
		'methods',
		'symbolTableRef',
		'alreadyPuttingObject',
		'pendingObjects',
		'classTableRef'
	],
	#classVars : [
		'BaseAddress',
		'ClassArray',
		'ClassCharacter',
		'ClassClass',
		'ClassFalse',
		'ClassFloat',
		'ClassMask',
		'ClassMethod',
		'ClassObject',
		'ClassSmallInteger',
		'ClassString',
		'ClassSymbol',
		'ClassTrue',
		'ClassUndefinedObject',
		'DirectIndexed',
		'DirectIndexedWithPointers',
		'External',
		'ExternalNonObject',
		'ExternalNonObjectWithPointers',
		'ExternalStruct',
		'ExternalWeakWithPointers',
		'ExternalWithPointers',
		'FirstAssignableClass',
		'FormatMask',
		'FormatShift',
		'Free',
		'HashMask',
		'HashShift',
		'Indexed',
		'IndexedNonObject',
		'IndexedNonObjectWithPointers',
		'IndexedStruct',
		'IndexedWeakWithPointers',
		'IndexedWithPointers',
		'ListOfClasses',
		'MaxHeaderLength',
		'NotIndexable',
		'NotIndexableWithPointers',
		'SizeMask',
		'SizeShift',
		'Special'
	],
	#category : 'ASTSmalltalk-Image',
	#package : 'ASTSmalltalk',
	#tag : 'Image'
}

{ #category : 'export' }
ASImage class >> exportMethods [
	^ self exportPackages flatCollect: [ :package |
			package methods
		]
]

{ #category : 'export' }
ASImage class >> exportPackages [
	^ RPackageOrganizer default packages select: [: package |
			((package name beginsWith: 'ASTSmalltalk')
					or: ((package name beginsWith: 'PC') 
								and: [(package name findString: '-Option') = 0]))
				and: [ (package name endsWith: '-Tests') not ]
		]
]

{ #category : 'instance creation' }
ASImage class >> image [

	^ super new initHeapHeader
]

{ #category : 'class initialization' }
ASImage class >> initialize [

	BaseAddress := 16r1000000000.
	SizeMask := 16r7fff.
	SizeShift := 48.
	FormatMask := 16r1f.
	FormatShift := 40.
	HashShift := 16.
	HashMask := 1 << (FormatShift - HashShift) - 1.
	ClassMask := 1 << HashShift - 1.
	MaxHeaderLength := 4093.
	ClassObject := 1.
	ClassSmallInteger := ClassObject + 1.
	ClassUndefinedObject := ClassSmallInteger + 1.
	ClassFalse := ClassUndefinedObject + 1.
	ClassTrue := ClassFalse + 1.
	ClassFloat := ClassTrue + 1.
	ClassSymbol := ClassFloat + 1.
	ClassCharacter := ClassSymbol + 1.
	ClassArray := ClassCharacter + 1.
	ClassString := ClassArray + 1.
	ClassClass := ClassString + 1.
	ClassMethod := ClassClass + 1.
	FirstAssignableClass := 30.
	ListOfClasses := {
		                 Object.
		                 SmallInteger.
		                 UndefinedObject.
		                 False.
		                 True.
		                 Float.
		                 Symbol.
		                 Character.
		                 Array.
		                 String.
		                 CompiledMethod.
		                 Class.
		                 Context.
		                 BlockClosure }.
	IndexedStruct := 110.
	ExternalStruct := IndexedStruct + 1.
	NotIndexable := ExternalStruct + 1.
	IndexedNonObject := NotIndexable + 1.
	ExternalNonObject := IndexedNonObject + 1.
	DirectIndexed := ExternalNonObject + 1.
	Indexed := DirectIndexed + 1.
	External := Indexed + 1.
	Free := External + 1.
	Special := Free + 1.
	NotIndexableWithPointers := Special + 1.
	IndexedNonObjectWithPointers := NotIndexableWithPointers + 1.
	ExternalNonObjectWithPointers := IndexedNonObjectWithPointers + 1.
	DirectIndexedWithPointers := ExternalNonObjectWithPointers + 1.
	IndexedWithPointers := DirectIndexedWithPointers + 1.
	ExternalWithPointers := IndexedWithPointers + 1.
	ExternalWeakWithPointers := ExternalWithPointers + 1.
	IndexedWeakWithPointers := ExternalWeakWithPointers + 1
]

{ #category : 'instance creation' }
ASImage class >> new [

	^ self error: 'use "image" or "test" to create instances of ',self name
]

{ #category : 'image analysis' }
ASImage class >> primitiveNumber: n [

	^ ((#primitive: senders flatCollect: [ :m | m pragmas ])
		    select: [ :p | p selector = #primitive: and: [p arguments first = n] ])
]

{ #category : 'image analysis' }
ASImage class >> primitiveNumbers [
^ (((#primitive: senders flatCollect: [ :m| m pragmas]) select: [:p| p selector = #primitive: ] thenCollect: [:p| p arguments first ]) asSet asArray select: #isInteger) asSortedCollection 
]

{ #category : 'image analysis' }
ASImage class >> primitiveNumbers: aCollection [

	^ ((#primitive: senders flatCollect: [ :m | m pragmas ]) select: [ :p |
		  p selector = #primitive: and: [
			  aCollection includes: p arguments first ] ]) asSortedCollection: [ :l :r | l arguments first <= r arguments first ]
]

{ #category : 'instance creation' }
ASImage class >> test [
	^ super new
]

{ #category : 'writing' }
ASImage >> addFirstMethod: selector in: canonicalClasses [
	canonicalClasses do: [: class |
		class methodDict at: selector ifPresent: [: method | self method: method. ^ self ]
	]

]

{ #category : 'writing' }
ASImage >> at: position put: anInteger [

	| savedPosition |
	savedPosition := aStream position.
	aStream position: position - BaseAddress.
	self putRawInt: anInteger.
	^ aStream position: savedPosition
]

{ #category : 'accessing' }
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

{ #category : 'adding' }
ASImage >> class: aClass [

	| cc |
	cc := (self canonicalClasses: aClass) first.
	^ classes at: cc ifAbsentPut: [
		  newClasses add: cc.
		  classes size + 1 ]
]

{ #category : 'writing' }
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

{ #category : 'compiling' }
ASImage >> compileMethod: aMethod [
	self halt
]

{ #category : 'writing' }
ASImage >> createClassTable [ 
	| table |
	table := Array new: classes size.
	classes keysAndValuesDo: [: class : number |
		table at: number+1 put: (self imageObject: class) ].
	^ table
]

{ #category : 'accessing' }
ASImage >> currentAddress [

	^ currentAddress
]

{ #category : 'adding' }
ASImage >> double: c [

	^ self subclassResponsibility
]

{ #category : 'objects' }
ASImage >> imageObject: anObject [ 
	^ anObject astImageObject: self
]

{ #category : 'writing' }
ASImage >> initHeapHeader [

	self putRawInt: 16r6567616d69545341.
	symbolTableRef := self putRawInt: 0.
	classTableRef := self putRawInt: 0.
	self initClasses
]

{ #category : 'initialization' }
ASImage >> initialize [

	super initialize.
	currentAddress := BaseAddress.
	objects := IdentityDictionary new.
	symbols := Dictionary new.
	classes := Dictionary new.
	methods := Dictionary new.
	selectors := Set new.
	newClasses := OrderedCollection new.
	newSelectors := OrderedCollection new.
	alreadyPuttingObject := false.
	pendingObjects := Dictionary new.
	#( #= value value: cull: yourself doesNotUnderstand:
	   #+ #- #* size at: at:put: #'~=' #'==' #'~~' value:value:
	   negated new new: value:value:value: value:value:value:value:
	   valueWithArguments: cull:cull: cull:cull:cull: cull:cull:cull:cull:
	   self name #< #'<=' #'>=' #> class Class Behavior ClassDescription
	   Metaclass SmallInteger noFallback ifTrue: ifTrue:ifFalse
	   ifFalse: ifFalse:ifTrue: ifNil: ifNil:ifNotNil ifNotNil: ifNotNil:ifNil:
	   perform: perform:with: perform:with:with: perform:with:with:with:
	   perform:withArguments: perform:withArguments:inSuperclass: ) do: [
		:symbol | self symbol: symbol ]. " has to match symbols.zig "
	{
		Object.
		SmallInteger.
		UndefinedObject.
		False.
		True.
		Float.
		Symbol.
		Character.
		Array.
		String.
		CompiledMethod.
		Class.
		Context.
		BlockClosure } do: [ :class | self class: class ]. " has to match zobject.zig "
	#( Method Dispatch ThunkSmallInteger ThunkFloat ThunkImmediate
	   ThunkReturnSelf ThunkReturnTrue ThunkReturnFalse ThunkReturnNil
	   ThunkReturn_1 ThunkReturn0 ThunkReturn1 ThunkReturn2 ThunkHeap )
		do: [ :className | classes at: className put: nil ]
]

{ #category : 'adding' }
ASImage >> literal: l class: c [

	^ self subclassResponsibility 
]

{ #category : 'writing' }
ASImage >> manifestObject: object [

	alreadyPuttingObject := true.
	object ifNotNil: [objects at: object put: (object astImagePut: self)].
	[ pendingObjects isEmpty ] whileFalse: [
		| tempPending |
		tempPending := pendingObjects.
		pendingObjects := Dictionary new.
		tempPending keysAndValuesDo: [ :obj :locations |
			| pointer |
			pointer := obj astImagePut: self.
			locations do: [ :each | self at: each put: pointer ] ] ].
	alreadyPuttingObject := false
]

{ #category : 'accessing' }
ASImage >> method: aCompiledMethod [
	^ methods at: aCompiledMethod ifAbsentPut: [ self compileMethod: aCompiledMethod ]
]

{ #category : 'accessing' }
ASImage >> methods [
	^ methods
]

{ #category : 'writing' }
ASImage >> nextPutAll: aByteArray [
	^ aStream nextPutAll: aByteArray.
]

{ #category : 'adding' }
ASImage >> nextRawInt [
	"input in little-endian order"

	| result |
	result := 0.
	0 to: 63 by: 8 do: [ :i | result := aStream next << i + result ].
	^ result
]

{ #category : 'objects' }
ASImage >> object: aObject fields: nFields elements: nElements [ 
	self shouldBeImplemented 
]

{ #category : 'adding' }
ASImage >> objectClass: aClass [

	| cc |
	cc := (self canonicalClasses: aClass) first.
	self halt.
	^ classes at: cc ifAbsentPut: [ classes size ]
]

{ #category : 'adding' }
ASImage >> objectMethod: aMethod [

	self halt
]

{ #category : 'adding' }
ASImage >> objectObject: anObject [

	| nInstVars nExtra ai class |
	class := anObject class.
	nInstVars := class allSlots size.
	nExtra := class isVariable ifTrue: [ anObject size ].
	ai := ASAllocationInfo
		      headerIVars: nInstVars
		      indexed: nExtra
		      element: nil
		      weak: false.
	^ ai put: anObject into: self
]

{ #category : 'adding' }
ASImage >> objectString: aString [

	| ai |
	ai := ASAllocationInfo
		      headerIVars: 0
		      indexed: aString size
		      element: 1
		      weak: false.
	self putRawInt: (ai header: ClassString).
	^ self putRawByteArray: aString asByteArray
]

{ #category : 'instance creation' }
ASImage >> on: aWriteStream [ 
	aStream := aWriteStream
]

{ #category : 'adding' }
ASImage >> pointer: p class: c [

	^ self subclassResponsibility
]

{ #category : 'process' }
ASImage >> processOut [

	^ self manifestObject: nil
]

{ #category : 'instance creation' }
ASImage >> put: value [

	| result |
	result := value astImageObject: self.
	self processOut.
	^ result
]

{ #category : 'writing' }
ASImage >> putBoolean: value [

	^ self putLiteral: value asBit class: ClassFalse + value asBit
]

{ #category : 'writing' }
ASImage >> putCharacter: aCharacter [

	^ self putLiteral: aCharacter asInteger class: ClassCharacter
]

{ #category : 'writing' }
ASImage >> putFloat: aFloat [

	^ self putRawInt: (self double: aFloat)
]

{ #category : 'writing' }
ASImage >> putLiteral: hash class: class [
	^ self putRawInt: (self literal: hash class: class)
]

{ #category : 'writing' }
ASImage >> putObject: object [

	alreadyPuttingObject ifTrue: [
		| location |
		location := self putPlaceholder.
		pendingObjects
			at: object
			ifPresent: [ :existing |
			pendingObjects at: object put: existing , { location } ]
			ifAbsentPut: [ { location } ].
		^ location ].
	objects
		at: object
		ifPresent: [ :pointer | ^ self putRawInt: pointer ].
	^ self manifestObject: object
]

{ #category : 'adding' }
ASImage >> putPlaceholder [

	^ self putRawInt: -1
]

{ #category : 'writing' }
ASImage >> putRawByteArray: aCollection [

	| length result |
	length := aCollection size - 1 | 7 + 1.
	aStream nextPutAll: aCollection.
	aCollection size to: length - 1 do: [ :ignored | aStream nextPut: 0 ].
	result := currentAddress.
	currentAddress := aStream position + BaseAddress.
	^ result
]

{ #category : 'writing' }
ASImage >> putRawInt: anInteger [
	"output in little-endian order"

	| result |
	1 to: 8 do: [ :i | aStream nextPut: (anInteger byteAt: i) ].
	result := currentAddress.
	currentAddress := aStream position + BaseAddress.
	^ result
]

{ #category : 'writing' }
ASImage >> putSmallInteger: anInteger [

	^ self putRawInt: (self smallInteger: anInteger)
]

{ #category : 'writing' }
ASImage >> putSymbol: aString [

	^ self putRawInt: (self symbol: aString)
]

{ #category : 'writing' }
ASImage >> putUndefinedObject [

	^ self putLiteral: 16rffffffff class: ClassUndefinedObject
]

{ #category : 'writing' }
ASImage >> rawIntAt: position [

	| savedPosition result |
	savedPosition := aStream position.
	aStream position: position * 8.
	result := self nextRawInt.
	aStream position: savedPosition.
	^ result
]

{ #category : 'adding' }
ASImage >> selector: aSymbol [
	^ selectors addIfNotPresent: aSymbol andDo:  [ newSelectors add: aSymbol ].

]

{ #category : 'adding' }
ASImage >> smallInteger: c [

	^ self subclassResponsibility
]

{ #category : 'adding' }
ASImage >> symbol: aString [

	aString isSymbol ifFalse: [ ^ self symbol: aString asSymbol ].
	^ self
		  literal: (symbols at: aString ifAbsentPut: [ symbols size + 1 ]) <<8
			  + aString numArgs
		  class: ClassSymbol
]

{ #category : 'writing' }
ASImage >> writeClassTable [

	self at: classTableRef put: (self putRawInt: classes size).
	classes keysDo: [ :key | self put: key ]
]

{ #category : 'writing' }
ASImage >> writeHeap [

	self writeSymbolTable.
	self writeClassTable
]

{ #category : 'writing' }
ASImage >> writeSymbolTable [

	self at: symbolTableRef put: (self putRawInt: symbols size).
	symbols keysDo: [ :key | self put: key ]
]
