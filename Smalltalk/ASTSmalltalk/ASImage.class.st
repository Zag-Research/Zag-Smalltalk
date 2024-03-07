"
I am the abstract class to write out Zag-Smalltalk images.

I have subclasses to output the two supported encodings: nan-encoded and tag-encoded (modified Spur).
"
Class {
	#name : 'ASImage',
	#superclass : 'Object',
	#instVars : [
		'symbols',
		'classes',
		'baseAddress',
		'aStream',
		'currentAddress',
		'pendingObjects',
		'symbolTableRef',
		'classTableRef'
	],
	#classVars : [
		'BaseAddress',
		'ClassASArg',
		'ClassASReturn',
		'ClassASSend',
		'ClassASSequence',
		'ClassASSuper',
		'ClassArray',
		'ClassBlockClosure',
		'ClassBlockFailure',
		'ClassCharacter',
		'ClassClass',
		'ClassCompiledMethod',
		'ClassContext',
		'ClassContextData',
		'ClassDispatch',
		'ClassFalse',
		'ClassFloat',
		'ClassMethod',
		'ClassObject',
		'ClassSmallInteger',
		'ClassString',
		'ClassSymbol',
		'ClassSymbolTable',
		'ClassThunkFloat',
		'ClassThunkHeap',
		'ClassThunkImmediate',
		'ClassThunkRet',
		'ClassThunkReturn',
		'ClassThunkReturn0',
		'ClassThunkReturn1',
		'ClassThunkReturn2',
		'ClassThunkReturnFalse',
		'ClassThunkReturnNil',
		'ClassThunkReturnSelf',
		'ClassThunkReturnTrue',
		'ClassThunkSmallInteger',
		'ClassTrue',
		'ClassUndefinedObject',
		'ThunkReturn_Class1',
		'_1'
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
ASImage >> at: position do: aBlock [

	| savedPosition |
	savedPosition := aStream position.
	aStream position: position-baseAddress.
	aBlock value.
	^ aStream position: savedPosition.

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
ASImage >> header: header [

	^ self subclassResponsibility
]

{ #category : 'initialization' }
ASImage >> initClasses [

	ClassObject := 1.
	ClassSmallInteger := 2.
	ClassUndefinedObject := 3.
	ClassFalse := 4.
	ClassTrue := 5.
	ClassFloat := 6.
	ClassSymbol := 7.
	ClassCharacter := 8.
	ClassBlockClosure := 9.
	ClassThunkSmallInteger := 10.
	ClassThunkFloat := 11.
	ClassThunkImmediate := 12.
	ClassThunkHeap := 13.
	ClassThunkReturnSelf := 14.
	ClassThunkReturnTrue := 15.
	ClassThunkReturnFalse := 16.
	ClassThunkReturnNil := 17.
	ThunkReturn_Class1 := 18.
	ClassThunkReturn0 := 19.
	ClassThunkReturn1 := 20.
	ClassThunkReturn2 := 21.
	ClassArray := 22.
	ClassString := 23.
	ClassSymbolTable := 24.
	ClassMethod := 25.
	ClassCompiledMethod := 26.
	ClassDispatch := 27.
	ClassContextData := 28.
	ClassContext := 29.
	ClassBlockFailure := 30.
	ClassClass := 31.
	ClassASSuper := 32.
	ClassASArg := 33.
	ClassASSend := 34.
	ClassASReturn := 35.
	ClassASSequence := 36.
	{
		Object.
		BlockClosure.
		False.
		True.
		UndefinedObject.
		SmallInteger.
		Symbol.
		Character.
		Float. " all of those above must be in this order because they are the immediate/literal classes "
		ASTSystem } do: [ :class | self put: class ]
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
	symbols := OrderedDictionary new.
	classes := OrderedDictionary new.
	pendingObjects := Dictionary new.
	baseAddress := BaseAddress.
	currentAddress := baseAddress + 16.
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

]

{ #category : 'adding' }
ASImage >> literal: l class: c [

^ self subclassResponsibility 
]

{ #category : 'accessing' }
ASImage >> offset [
	^ currentAddress - baseAddress
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

{ #category : 'adding' }
ASImage >> putClass: aClass [

	| cc |
	cc := (self canonicalClasses: aClass) first.
	^ classes at: cc ifAbsentPut: [ classes size ]
]

{ #category : 'writing' }
ASImage >> putLiteral: hash class: class [
	^ self putRawInt: (self literal: hash class: class)
]

{ #category : 'writing' }
ASImage >> putObject: value [

	^ self halt. "literal: (anASImage generalObject: self) address >> 3 class: 0"
]

{ #category : 'adding' }
ASImage >> putRawInt: anInteger [

	^ aStream nextNumber: 8 put: anInteger 
]

{ #category : 'adding' }
ASImage >> putString: aString [

	self halt
]

{ #category : 'adding' }
ASImage >> putSymbol: aString [

	^ self putRawInt: (self symbol: aString)
]

{ #category : 'adding' }
ASImage >> putUndefinedObject [

	^ self literal: 7 class: ClassUndefinedObject
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

	self at: classTableRef do: [ self putRawInt: currentAddress ].
	self putRawInt: classes size.
	classes keysDo: [ :key | self put: key ]
]

{ #category : 'writing' }
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

{ #category : 'writing' }
ASImage >> writeSymbolTable [

	self at: symbolTableRef do: [ self putRawInt: currentAddress ].
	self putRawInt: symbols size.
	symbols keysDo: [ :key | self put: key ]
]
