"
I write out Zag-Smalltalk images
"
Class {
	#name : 'ASImageExporter',
	#superclass : 'Object',
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
		'classTableRef',
		'currentHeap',
		'constants',
		'requireThreadedCode'
	],
	#pools : [
		'ZagConstants'
	],
	#classInstVars : [
		'imageFolder'
	],
	#category : 'Zag-Exporter-Exporting',
	#package : 'Zag-Exporter',
	#tag : 'Exporting'
}

{ #category : 'export' }
ASImageExporter class >> exportMethods [
	^ self exportPackages flatCollect: [ :package |
			package methods
		]
]

{ #category : 'export' }
ASImageExporter class >> exportPackages [

	^ PackageOrganizer default packages select: [ :package |
		  ((package name beginsWith: 'Zag-Smalltalk') or:
			   ((package name beginsWith: 'PC') and: [
				    (package name findString: '-Option') = 0 ])) and: [
			  (package name endsWith: '-Tests') not ] ]
]

{ #category : 'export' }
ASImageExporter class >> ifInvalidFolder: folder do: aBlock [
	
	(folder isNil or: [folder exists not]) ifTrue: aBlock
]

{ #category : 'instance creation' }
ASImageExporter class >> image [

	^ super new initHeapHeader
]

{ #category : 'export' }
ASImageExporter class >> imageFolder: anObject [
	
	imageFolder := anObject
]

{ #category : 'instance creation' }
ASImageExporter class >> new [

	^ self error: 'use "image" or "test" to create instances of ',self name
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

{ #category : 'accessing' }
ASImageExporter >> constants [
	^ constants
]

{ #category : 'initialization' }
ASImageExporter >> constants: constantsClass [

	constants := constantsClass.
	currentAddress := BaseAddress.
	#( #= value value: cull: yourself doesNotUnderstand:
	   #+ #- #* size at: at:put: #'~=' #'==' #'~~' value:value:
	   negated new new: value:value:value: value:value:value:value:
	   valueWithArguments: cull:cull: cull:cull:cull: cull:cull:cull:cull:
	   self name #< #'<=' #'>=' #> class Class Behavior ClassDescription
	   Metaclass SmallInteger noFallback ifTrue: ifTrue:ifFalse
	   ifFalse: ifFalse:ifTrue: ifNil: ifNil:ifNotNil ifNotNil: ifNotNil:ifNil:
	   perform: perform:with: perform:with:with: perform:with:with:with:
	   perform:withArguments: perform:withArguments:inSuperclass: ) do: [
		:symbol | self objectSymbol: symbol ]. " has to match symbols.zig "

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
	objects := IdentityDictionary new.
	symbols := Dictionary new.
	classes := Dictionary new.
	methods := Dictionary new.
	selectors := Set new.
	newClasses := OrderedCollection new.
	newSelectors := OrderedCollection new.
	requireThreadedCode := false.
	{
		#ThunkReturnHeap.
		#ThunkReturnLocal.
		#ThunkReturnInstance.
		#ThunkReturnSmallInteger.
		#ThunkReturnImmediate.
		#ThunkReturnCharacter.
		#BlockAssignLocal.
		#BlockAssignInstance.
		#ThunkGetInstance.
		#ThunkImmediate.
		#ThunkFloat.
		False.
		True.
		SmallInteger.
		Symbol.
		Character.
		#ShortString.
		#UnUsed18. #UnUsed19. #UnUsed20. #UnUsed21. #UnUsed22. #UnUsed23. #UnUsed24. #UnUsed25. #UnUsed26. #UnUsed27. #UnUsed28. #UnUsed29. #UnUsed30. #UnUsed31. 
		UndefinedObject.
		Float.
		ProtoObject.
		Object.
		BlockClosure.
		#BlockClosureValue.
		Context.
		Array.
		String.
		#Utf8String.
		DoubleWordArray.
		Process.
		Class.
		CompiledMethod.
		#Dispatch
 } do: [ :class | self class: class ]. " has to match zobject.zig "

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

	^ constants boolean: value
]

{ #category : 'writing' }
ASImageExporter >> objectCharacter: aCharacter [

	^ self
		  objectLiteral: aCharacter asInteger
		  class: ClassCharacter
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

	^ constants double: aFloat for: self
]

{ #category : 'writing' }
ASImageExporter >> objectLiteral: hash class: class [

	^ constants literal: hash class: class
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

	^ constants smallInteger: anInteger
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
		with: aString asByteArray asU64Array
		do: [ :chunk | chunk ].
	^ self pointer: result
]

{ #category : 'writing' }
ASImageExporter >> objectSymbol: aString [

	aString isSymbol ifFalse: [ ^ self objectSymbol: aString asSymbol ].
	^ constants
		  literal:
			  (symbols at: aString ifAbsentPut: [ symbols size + 1 ]) << 8
			  + aString numArgs
		  class: ClassSymbol
]

{ #category : 'writing' }
ASImageExporter >> objectUndefinedObject [

	^ constants undefinedObject
]

{ #category : 'adding' }
ASImageExporter >> pointer: p [

	^ constants pointer: p
]

{ #category : 'writing' }
ASImageExporter >> putU64: anInteger [
	"output in little-endian order"

	(currentHeap hasSpaceFor: 1) ifFalse: [
		  self addSubHeap].
	^ currentHeap addU64: anInteger
]

{ #category : 'enumerating' }
ASImageExporter >> requiringThreadedCodeDo: aBlock [

	requireThreadedCode := true.
	aBlock value.
	requireThreadedCode := false
]

{ #category : 'adding' }
ASImageExporter >> selector: aSymbol [
	^ selectors add: aSymbol andIfNewDo:  [ newSelectors add: aSymbol ].

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
