"
I write out ASTSmalltalk images
"
Class {
	#name : 'ASImageExporter',
	#superclass : 'SharedPool',
	#instVars : [
		'symbols',
		'classes',
		'currentAddress',
		'objects',
		'process',
		'newClasses',
		'selectors',
		'newSelectors',
		'methods',
		'symbolTableRef',
		'pendingObjects',
		'classTableRef',
		'currentHeap'
	],
	#classVars : [
		'BaseAddress',
		'ClassArray',
		'ClassCharacter',
		'ClassClass',
		'ClassDoubleWordArray',
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
	#classInstVars : [
		'imageFolder'
	],
	#category : 'ASTSmalltalk-Image',
	#package : 'ASTSmalltalk',
	#tag : 'Image'
}

{ #category : 'converting' }
ASImageExporter class >> convertToZag: anObject [

	^ anObject convertToZag: self
]

{ #category : 'converting' }
ASImageExporter class >> double: aNumber for: anImage [

	^ self subclassResponsibility
]

{ #category : 'export' }
ASImageExporter class >> exportMethods [
	^ self exportPackages flatCollect: [ :package |
			package methods
		]
]

{ #category : 'export' }
ASImageExporter class >> exportPackages [
	^ RPackageOrganizer default packages select: [: package |
			((package name beginsWith: 'ASTSmalltalk')
					or: ((package name beginsWith: 'PC') 
								and: [(package name findString: '-Option') = 0]))
				and: [ (package name endsWith: '-Tests') not ]
		]
]

{ #category : 'as yet unclassified' }
ASImageExporter class >> ifInvalidFolder: folder do: aBlock [
	
	(folder isNil or: [folder exists not]) ifTrue: aBlock
]

{ #category : 'instance creation' }
ASImageExporter class >> image [

	^ super new initHeapHeader
]

{ #category : 'accessing' }
ASImageExporter class >> imageFolder: anObject [
	
	imageFolder := anObject
]

{ #category : 'class initialization' }
ASImageExporter class >> initialize [

	BaseAddress := 16r1000000000.
	SizeMask := 16r7fff.
	SizeShift := 48.
	FormatMask := 16r1f.
	FormatShift := 40.
	HashShift := 16.
	HashMask := 1 << (FormatShift - HashShift) - 1.
	ClassMask := 1 << HashShift - 1.
	MaxHeaderLength := 4093.
	ClassFalse := 10.
	ClassTrue := ClassFalse + 1.
	ClassSmallInteger := 12.
	ClassSymbol := 13.
	ClassCharacter := 14.
	ClassUndefinedObject := 23.
	ClassFloat := 24.
	ClassObject := 26.
	ClassArray := 30.
	ClassString := 31.
"	ClassClass := ClassString + 1.
	ClassMethod := ClassClass + 1."
	FirstAssignableClass := 35.
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

{ #category : 'converting' }
ASImageExporter class >> literal: l class: c [

	^ self subclassResponsibility
]

{ #category : 'instance creation' }
ASImageExporter class >> new [

	^ self error: 'use "image" or "test" to create instances of ',self name
]

{ #category : 'converting' }
ASImageExporter class >> pointer: p [

	^ self subclassResponsibility
]

{ #category : 'image analysis' }
ASImageExporter class >> primitiveNumber: n [

	^ ((#primitive: senders flatCollect: [ :m | m pragmas ])
		    select: [ :p | p selector = #primitive: and: [p arguments first = n] ])
]

{ #category : 'image analysis' }
ASImageExporter class >> primitiveNumbers [
^ (((#primitive: senders flatCollect: [ :m| m pragmas]) select: [:p| p selector = #primitive: ] thenCollect: [:p| p arguments first ]) asSet asArray select: #isInteger) asSortedCollection 
]

{ #category : 'image analysis' }
ASImageExporter class >> primitiveNumbers: aCollection [

	^ ((#primitive: senders flatCollect: [ :m | m pragmas ]) select: [ :p |
		  p selector = #primitive: and: [
			  aCollection includes: p arguments first ] ]) asSortedCollection: [ :l :r | l arguments first <= r arguments first ]
]

{ #category : 'initialization' }
ASImageExporter class >> setUpImageFolder [
	
	| folder |
	folder := UIManager default chooseDirectory: self setUpImageFolderPrompt.
	self ifInvalidFolder: folder do: [  
		^self error: 'Image: Invalid folder for exporting image'].
	self imageFolder: folder.
]

{ #category : 'initialization' }
ASImageExporter class >> setUpImageFolderPrompt [

	^ 'Folder for image export please?'
]

{ #category : 'converting' }
ASImageExporter class >> smallInteger: aNumber [

	^ self subclassResponsibility
]

{ #category : 'writing' }
ASImageExporter >> addFirstMethod: selector in: canonicalClasses [
	canonicalClasses do: [: class |
		class methodDict at: selector ifPresent: [: method | self method: method. ^ self ]
	]

]

{ #category : 'writing' }
ASImageExporter >> addSubHeap [

	currentHeap := (currentHeap ifNil: [ ASSubHeap ]) addStorage: self
]

{ #category : 'accessing' }
ASImageExporter >> at: position put: anInteger [

	self halt
]

{ #category : 'accessing' }
ASImageExporter >> canonicalClasses: aClass [
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
ASImageExporter >> class: aClass [

	| cc |
	cc := (self canonicalClasses: aClass) first.
	^ classes at: cc ifAbsentPut: [
		  newClasses add: cc.
		  classes size + 1 ]
]

{ #category : 'writing' }
ASImageExporter >> closeOverClassesAndSelectors [

	| tempClasses tempSelectors |
	[ newClasses size + newSelectors size > 0 ] whileTrue: [
		tempClasses := newClasses.
		newClasses := OrderedCollection new.
		tempSelectors := newSelectors.
		newSelectors := OrderedCollection new.
		tempClasses do: [ :class |
			| cc |
			cc := self canonicalClasses: class.
			cc first superclass ifNotNil: [ :superclass |
				superclass astImageObject: self ].
			cc first astImageObject: self.
			selectors do: [ :selector | self addFirstMethod: selector in: cc ] ].
		tempSelectors do: [ :selector |
			classes keysDo: [ :class |
				self addFirstMethod: selector in: (self canonicalClasses: class) ] ] ]
]

{ #category : 'compiling' }
ASImageExporter >> compileMethod: aMethod [
	self halt
]

{ #category : 'writing' }
ASImageExporter >> createClassTable [

	| table |
	table := Array new: classes size.
	classes keysAndValuesDo: [ :class :number |
		table at: number + 1 put: (class astImageObject: self) ].
	^ table
]

{ #category : 'tests' }
ASImageExporter >> currentHeap [

	^ currentHeap
]

{ #category : 'initialization' }
ASImageExporter >> initialize [

	super initialize.
	currentAddress := BaseAddress.
	objects := IdentityDictionary new.
	symbols := Dictionary new.
	classes := Dictionary new.
	methods := Dictionary new.
	selectors := Set new.
	newClasses := OrderedCollection new.
	newSelectors := OrderedCollection new.
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

{ #category : 'accessing' }
ASImageExporter >> method: aCompiledMethod [
	^ methods at: aCompiledMethod ifAbsentPut: [ self compileMethod: aCompiledMethod ]
]

{ #category : 'accessing' }
ASImageExporter >> methods [
	^ methods
]

{ #category : 'writing' }
ASImageExporter >> objectBoolean: value [

	^ self class boolean: value
]

{ #category : 'writing' }
ASImageExporter >> objectCharacter: aCharacter [

	^ self objectLiteral: aCharacter asInteger class: ClassCharacter
]

{ #category : 'adding' }
ASImageExporter >> objectClass: aClass [

	| cc |
	cc := (self canonicalClasses: aClass) first.
	self shouldBeImplemented.
	^ classes at: cc ifAbsentPut: [ classes size ]
]

{ #category : 'writing' }
ASImageExporter >> objectFloat: aFloat [

	^ self class double: aFloat for: self
]

{ #category : 'writing' }
ASImageExporter >> objectLiteral: hash class: class [

	^ self class literal: hash class: class
]

{ #category : 'adding' }
ASImageExporter >> objectMethod: aMethod [

	self shouldBeImplemented
]

{ #category : 'adding' }
ASImageExporter >> objectObject: anObject [

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

{ #category : 'writing' }
ASImageExporter >> objectSmallInteger: anInteger [

	^ self class smallInteger: anInteger
]

{ #category : 'adding' }
ASImageExporter >> objectString: aString [

	| ai result |
	ai := ASAllocationInfo
		      headerIVars: 0
		      indexed: aString size
		      element: 1
		      weak: false.
	result := ai header: ClassString into: self.
	currentHeap
		at: result
		with: (aString asByteArray asU64Array)
		do: [ :chunk | chunk ].
	^ self pointer: result
]

{ #category : 'writing' }
ASImageExporter >> objectSymbol: aString [

	^ self symbol: aString
]

{ #category : 'writing' }
ASImageExporter >> objectUndefinedObject [

	^ self class undefinedObject
]

{ #category : 'adding' }
ASImageExporter >> pointer: p [

	^ self class pointer: p
]

{ #category : 'writing' }
ASImageExporter >> putU64: anInteger [
	"output in little-endian order"

	(currentHeap hasSpaceFor: 1) ifFalse: [
		  self addSubHeap].
	^ currentHeap addU64: anInteger
]

{ #category : 'adding' }
ASImageExporter >> selector: aSymbol [
	^ selectors add: aSymbol andIfNewDo:  [ newSelectors add: aSymbol ].

]

{ #category : 'adding' }
ASImageExporter >> symbol: aString [

	aString isSymbol ifFalse: [ ^ self symbol: aString asSymbol ].
	^ self class
		  literal:
			  (symbols at: aString ifAbsentPut: [ symbols size + 1 ]) << 8
			  + aString numArgs
		  class: ClassSymbol
]

{ #category : 'writing' }
ASImageExporter >> writeClassTable [

	self halt
]

{ #category : 'writing' }
ASImageExporter >> writeHeap [

	self writeSymbolTable.
	self writeClassTable
]

{ #category : 'writing' }
ASImageExporter >> writeSymbolTable [

	self halt
]
