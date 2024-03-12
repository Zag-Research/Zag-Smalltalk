"
I am the abstract class to write out Zag-Smalltalk images.

I have subclasses to output the two supported encodings: nan-encoded and tag-encoded (modified Spur).
"
Class {
	#name : 'ASImage',
	#superclass : 'SharedPool',
	#instVars : [
		'symbols',
		'classes',
		'aStream',
		'currentAddress',
		'pendingObjects',
		'symbolTableRef',
		'classTableRef',
		'alreadyPuttingObject',
		'objects',
		'classDict'
	],
	#classVars : [
		'BaseAddress',
		'ClassASArg',
		'ClassArray',
		'ClassCharacter',
		'ClassClass',
		'ClassCompiledMethod',
		'ClassFalse',
		'ClassFloat',
		'ClassMethod',
		'ClassObject',
		'ClassSmallInteger',
		'ClassString',
		'ClassSymbol',
		'ClassThunkFloat',
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
		'Free',
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
		'Special'
	],
	#category : 'ASTSmalltalk-Image',
	#package : 'ASTSmalltalk',
	#tag : 'Image'
}

{ #category : 'as yet unclassified' }
ASImage class >> ClassArray [
	^ ClassArray
]

{ #category : 'as yet unclassified' }
ASImage class >> ClassObject [
	^ ClassObject
]

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
		                 BlockClosure.
		                 False.
		                 True.
		                 UndefinedObject.
		                 SmallInteger.
		                 Symbol.
		                 Character.
		                 Float. " all of those above must be in this order because they are the immediate/literal classes "
		                 ASTSystem }.
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

{ #category : 'tests' }
ASImage class >> test [
	^ super new
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

{ #category : 'instance creation' }
ASImage >> class: aClass [

	^ aClass astClass
		  ifNil: [
			  classDict
				  at: aClass
				  ifAbsentPut: [ classes add: aClass at: classes size ] ]
		  ifNotNil: [ :c |
			  classes add: aClass at: c.
			  c ]
]

{ #category : 'accessing' }
ASImage >> currentAddress [

	^ currentAddress
]

{ #category : 'adding' }
ASImage >> headerIVars: iVars indexed: nElements element: element weak: makeWeak [

	nElements ifNotNil: [
		| arraySize |
		arraySize := nElements * (element ifNil: [ 8 ]) + 7 // 8.
		makeWeak ifTrue: [
			iVars + arraySize + 3 > MaxHeaderLength ifTrue: [
				^ ASAllocationInfo new
					  format: ExternalWeakWithPointers;
					  instVars: iVars;
					  footerLength: 3 ].
			^ ASAllocationInfo new
				  format: IndexedWeakWithPointers;
				  instVars: iVars;
				  extra: arraySize;
				  footerLength: 3 ].
		iVars == 0 ifTrue: [
			(nElements == 0 or: [ element = 1 and: [ nElements < 110 ]])
				ifTrue: [
					^ ASAllocationInfo new
						  format: nElements;
						  extra: arraySize ].
			element ifNil: [
				nElements <= MaxHeaderLength ifTrue: [
					^ ASAllocationInfo new
						  format: DirectIndexed;
						  extra: arraySize ] ] ].
		iVars + arraySize > (MaxHeaderLength - 2) ifTrue: [
			^ ASAllocationInfo new
				  format: (element = Object
						   ifTrue: [ External ]
						   ifFalse: [ ExternalNonObject ]);
				  instVars: iVars;
				  footerLength: 2 ].
		^ ASAllocationInfo new
			  format: (element = Object
					   ifTrue: [ Indexed ]
					   ifFalse: [ IndexedNonObject ]);
			  instVars: iVars;
			  extra: arraySize;
			  footerLength: 2 ].
	^ ASAllocationInfo new
		  format: NotIndexable;
		  instVars: iVars
]

{ #category : 'initialization' }
ASImage >> initClasses [

	classes := OrderedDictionary new.
	ClassObject := 1.
	ClassSmallInteger := 2.
	ClassUndefinedObject := 3.
	ClassFalse := 4.
	ClassTrue := 5.
	ClassFloat := 6.
	ClassSymbol := 7.
	ClassCharacter := 8.
	ListOfClasses do: [ :class | self put: class ]
]

{ #category : 'writing' }
ASImage >> initHeapHeader [

	self putRawInt: 16r6567616d69545341.
	symbolTableRef := currentAddress.
	self putRawInt: 0.
	classTableRef := currentAddress.
	self putRawInt: 0.
	self initClasses
]

{ #category : 'initialization' }
ASImage >> initialize [

	super initialize.
	alreadyPuttingObject := false.
	objects := Dictionary new.
	pendingObjects := Dictionary new.
	symbols := OrderedDictionary new.
	classes := OrderedCollection
		           new: FirstAssignableClass - 1
		           withAll: nil.
	classDict := Dictionary new.
	currentAddress := BaseAddress.
	#( #= value value: cull: yourself doesNotUnderstand:
	   #+ #- #* size at: at:put: #'~=' #'==' #'~~' value:value:
	   negated new new: value:value:value: value:value:value:value:
	   valueWithArguments: cull:cull: cull:cull:cull: cull:cull:cull:cull:
	   self name #< #'<=' #'>=' #> class Class Behavior ClassDescription
	   Metaclass SmallInteger noFallback ifTrue: ifTrue:ifFalse
	   ifFalse: ifFalse:ifTrue: ifNil: ifNil:ifNotNil ifNotNil: ifNotNil:ifNil:
	   perform: perform:with: perform:with:with: perform:with:with:with:
	   perform:withArguments: perform:withArguments:inSuperclass:
	   Object ) do: [ :symbol | self symbol: symbol ].
	ClassObject := 1.
	ClassSmallInteger := 2.
	ClassUndefinedObject := 3.
	ClassFalse := 4.
	ClassTrue := 5.
	ClassFloat := 6.
	ClassSymbol := 7.
	ClassCharacter := 8
]

{ #category : 'adding' }
ASImage >> literal: l class: c [

^ self subclassResponsibility 
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
	nExtra := class isVariable ifTrue: [anObject size].
	ai := self headerIVars: nInstVars indexed: nExtra element: nil weak: false.
	^ ai put: anObject into: self
]

{ #category : 'adding' }
ASImage >> objectString: aString [

	| ai |
	ai := self headerIVars: 0 indexed: aString size element: 1 weak: false.
	self putRawInt: (ai header: ClassString ).
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

{ #category : 'writing' }
ASImage >> put: value [

	^ value astImageObject: self
]

{ #category : 'writing' }
ASImage >> putBoolean: value [

	^ self putLiteral: value asBit class: ClassFalse + value asBit
]

{ #category : 'adding' }
ASImage >> putCharacter: aCharacter [

	^ self putLiteral: aCharacter asInteger class: ClassCharacter
]

{ #category : 'writing' }
ASImage >> putLiteral: hash class: class [
	^ self putRawInt: (self literal: hash class: class)
]

{ #category : 'writing' }
ASImage >> putObject: object [

	alreadyPuttingObject ifTrue: [
		^ pendingObjects
			  at: object
			  ifPresent: [ :existing |
				  pendingObjects
					  at: object
					  put: existing , { self putPlaceholder } ]
			  ifAbsentPut: [ { self putPlaceholder } ] ].
	objects
		at: object
		ifPresent: [ :pointer | ^ self putRawInt: pointer ].
	alreadyPuttingObject := true.
	objects at: object put: (object astImagePut: self).
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

{ #category : 'adding' }
ASImage >> putPlaceholder [

	| result |
	result := currentAddress.
	1 to: 8 do: [ :i | aStream nextPut: 255 ].
	currentAddress := currentAddress + 8.
	^ result
]

{ #category : 'as yet unclassified' }
ASImage >> putRawByteArray: aCollection [

	| length |
	length := (aCollection size - 1 | 7) + 1.
	aStream nextPutAll: aCollection.
	aCollection size to: length - 1 do: [ : ignored | aStream nextPut: 0 ].
	currentAddress := currentAddress + length
]

{ #category : 'adding' }
ASImage >> putRawInt: anInteger [
	"output in little-endian order"

	1 to: 8 do: [ :i | aStream nextPut: (anInteger byteAt: i) ].
	currentAddress := currentAddress + 8
]

{ #category : 'adding' }
ASImage >> putSymbol: aString [

	^ self putRawInt: (self symbol: aString)
]

{ #category : 'adding' }
ASImage >> putUndefinedObject [

	^ self putLiteral: 16rffffffff class: ClassUndefinedObject
]

{ #category : 'adding' }
ASImage >> symbol: aString [

	aString isSymbol ifFalse: [ ^ self symbol: aString asSymbol ].
	^ self
		  literal:
			  (symbols at: aString ifAbsentPut: [ symbols size + 1 ]) << 8
			  + aString numArgs
		  class: ClassSymbol
]

{ #category : 'writing' }
ASImage >> writeClassTable [

	self at: classTableRef put: currentAddress.
	self putRawInt: classes size.
	classes keysDo: [ :key | self put: key ]
]

{ #category : 'writing' }
ASImage >> writeSymbolTable [

	self at: symbolTableRef put: currentAddress.
	self putRawInt: symbols size.
	symbols keysDo: [ :key | self put: key ]
]
