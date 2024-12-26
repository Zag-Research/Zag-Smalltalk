"
I write out Zag-Smalltalk images
"
Class {
	#name : 'ASImageExporter',
	#superclass : 'Object',
	#instVars : [
		'symbols',
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
		'requireThreadedCode',
		'extraSymbols',
		'classIndices',
		'classes',
		'lastClassIndex',
		'subHeaps',
		'dispatchTable'
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

{ #category : 'accessing' }
ASImageExporter class >> heapClass [

	^ ASSubHeap
]

{ #category : 'export' }
ASImageExporter class >> ifInvalidFolder: folder do: aBlock [
	
	(folder isNil or: [folder exists not]) ifTrue: aBlock
]

{ #category : 'export' }
ASImageExporter class >> imageFolder [

	imageFolder ifNil: [ self setUpImageFolder ].
	^ imageFolder
]

{ #category : 'export' }
ASImageExporter class >> imageFolder: anObject [
	
	imageFolder := anObject
]

{ #category : 'initialization' }
ASImageExporter class >> setUpImageFolder [

	| folder |
	<script>
	folder := UIManager default chooseDirectory:
		          self setUpImageFolderPrompt.
	self
		ifInvalidFolder: folder
		do: [ ^ self error: 'Image: Invalid folder for exporting image' ].
	self imageFolder: folder
]

{ #category : 'initialization' }
ASImageExporter class >> setUpImageFolderPrompt [

	^ 'Folder for image export please?'
]

{ #category : 'writing' }
ASImageExporter class >> writeImage [

	<script>
	self new
		constants: ZagImageEncodeZag;
		writeImageSending: #size to: #writeImage:
]

{ #category : 'writing' }
ASImageExporter >> addFirstMethod: selector in: canonicalClasses [
	canonicalClasses do: [: class |
		class methodDict at: selector ifPresent: [: method | self method: method. ^ self ]
	]

]

{ #category : 'writing' }
ASImageExporter >> addSubHeap [

	currentHeap := currentHeap
		               ifNil: [ self class heapClass new exporter: self ]
		               ifNotNil: [ currentHeap addStorage ].
	subHeaps add: currentHeap
]

{ #category : 'adding' }
ASImageExporter >> classIndex: aClass [

	^ classIndices at: aClass ifAbsentPut: [ lastClassIndex := lastClassIndex + 1 ]
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
				superclass encodedBy: self ].
			cc first encodedBy: self.
			selectors do: [ :selector | self addFirstMethod: selector in: cc ] ].
		tempSelectors do: [ :selector |
			classIndices keysDo: [ :class |
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
	table := Array new: lastClassIndex.
	classIndices keysAndValuesDo: [ :class :number |
		table at: number put: (class zagClass encodedBy: self) ].
	^ table
]

{ #category : 'tests' }
ASImageExporter >> currentHeap [

	^ currentHeap
]

{ #category : 'adding' }
ASImageExporter >> findHeapWithSpaceFor: size [

	(currentHeap hasSpaceFor: size) ifFalse: [
		self addSubHeap ].
	^ currentHeap
]

{ #category : 'export' }
ASImageExporter >> imageFolder [

	^ self class imageFolder
]

{ #category : 'writing' }
ASImageExporter >> imageSending: aSymbol to: anObject [

	| header |
	header := ASImageHeader new.
	header send: (aSymbol encodedBy: self) to: (anObject encodedBy: self).
	header symbolTable:
		((extraSymbols asChunkedArraysOfSize: 127) encodedBy: self).
	header codeAddresses:
		(ZagConstants asDoubleWordArray encodedBy: self).
	header dispatchTable: (dispatchTable collect: [ : each | each encodedBy: self]).
	^ header
]

{ #category : 'initialization' }
ASImageExporter >> initClasses [

	classIndices := Dictionary new.
	{
		#ThunkReturnLocal.
		#ThunkReturnInstance.
		#ThunkReturnSmallInteger.
		#ThunkReturnImmediate.
		#ThunkReturnCharacter.
		#ThunkReturnFloat.
		#ThunkHeap.
		#ThunkLocal.
		#ThunkInstance.
		#BlockAssignLocal.
		#BlockAssignInstance.
		#ThunkImmediate.
		#ThunkFloat.
		False.
		True.
		SmallInteger.
		Symbol.
		Character.
		nil.
		nil.
		nil.
		nil.
		nil.
		nil.
		nil.
		nil.
		nil.
		nil.
		nil.
		nil.
		nil.
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
		#Dispatch } withIndexDo: [ :class :index |
		lastClassIndex := index.
		class ifNotNil: [ classIndices at: class put: index ] ]
]

{ #category : 'initialization' }
ASImageExporter >> initialize [

	super initialize.
	objects := IdentityDictionary new.
	classes := IdentityDictionary new.
	methods := IdentityDictionary new.
	selectors := Set new.
	newClasses := OrderedCollection new.
	newSelectors := OrderedCollection new.
	subHeaps := OrderedCollection new.
	dispatchTable := OrderedCollection new.
	requireThreadedCode := false.
	self addSubHeap.
	self initClasses
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

	^ constants literal: aCharacter asInteger class: ClassCharacter
]

{ #category : 'adding' }
ASImageExporter >> objectClass: aClass [

	| nInstVars ai class metaClass |
	classes at: aClass ifPresent: [ :it | ^ it ].
	class := aClass zagClass.
	metaClass := class class.
	nInstVars := metaClass allSlots size.
	ai := ASAllocationInfo headerIVars: nInstVars.
	classes at: aClass put: (ai reserve: aClass in: self).
	^ ai
		  put: class
		  into: self
		  ivBlock: [ :i | (i = 1 ifTrue: [ aClass name ]) encodedBy: self ]
]

{ #category : 'writing' }
ASImageExporter >> objectFloat: aFloat [

	^ constants double: aFloat for: self
]

{ #category : 'writing' }
ASImageExporter >> objectHeapFloat: aFloat [

	^ self shouldBeImplemented; flag: #todo
]

{ #category : 'writing' }
ASImageExporter >> objectMethod: aCompiledMethod [

	^ methods
		  at: aCompiledMethod
		  ifAbsentPut: [ self compileMethod: aCompiledMethod ]
]

{ #category : 'adding' }
ASImageExporter >> objectNonObjectArray: anObject [

	| ai |
	objects at: anObject ifPresent: [ :it | ^ it ].
	ai := ASAllocationInfo
		      headerIVars: anObject class allSlots size
		      indexed: anObject size.
	objects at: anObject put: (ai reserve: anObject in: self).
	^ ai put: anObject into: self indexBlock: [ : i | anObject at: i ]
]

{ #category : 'adding' }
ASImageExporter >> objectObject: anObject [

	| nInstVars nExtra ai class |
	objects at: anObject ifPresent: [ :it | ^ it ].
	class := anObject class.
	nInstVars := class allSlots size.
	nExtra := class isVariable ifTrue: [ anObject size ].
	ai := ASAllocationInfo headerIVars: nInstVars indexed: nExtra.
	objects at: anObject put: (ai
			 reserve: anObject in: self;
			 pointer).
	^ ai put: anObject into: self
]

{ #category : 'writing' }
ASImageExporter >> objectSmallInteger: anInteger [

	^ constants smallInteger: anInteger
]

{ #category : 'adding' }
ASImageExporter >> objectString: aString [

	^ objects at: aString ifAbsentPut: [
		  | ai |
		  ai := ASAllocationInfo
			        headerIVars: 0
			        indexed: aString size
			        element: Character.
		  ai reserveClassIndex: ClassString in: self.
		  currentHeap
			  at: ai address
			  with: aString asByteArray asU64Array
			  do: [ :chunk | chunk ].
		  ai pointer ]
]

{ #category : 'writing' }
ASImageExporter >> objectSymbol: aString [

	aString isSymbol ifFalse: [ ^ self objectSymbol: aString asSymbol ].
	^ self symbols at: aString ifAbsentPut: [
		  extraSymbols add: aString.
		  constants
			  literal: aString numArgs << 24
				  + (symbols size + 1 * 10368889 bitAnd: 16rffffff)
			  class: ClassSymbol ]
]

{ #category : 'writing' }
ASImageExporter >> objectUndefinedObject [

	^ constants undefinedObject
]

{ #category : 'adding' }
ASImageExporter >> pointer: p [

	^ constants pointer: p
]

{ #category : 'enumerating' }
ASImageExporter >> requiringThreadedCodeDo: aBlock [

	requireThreadedCode := true.
	[aBlock value] ensure: [requireThreadedCode := false]
]

{ #category : 'adding' }
ASImageExporter >> selector: aSymbol [
	^ selectors add: aSymbol andIfNewDo:  [ newSelectors add: aSymbol ].

]

{ #category : 'initialization' }
ASImageExporter >> symbols [

	" add these symbols to the dictionary, but then remove them from the extraSymbols collection because the zag runtime already knows about them "
	symbols ifNotNil: [ ^ symbols ].
	symbols := Dictionary new.
	extraSymbols := OrderedCollection new: 500.
	#( #= value value: cull: yourself doesNotUnderstand:
	   #+ #- #* size at: at:put: #'~=' #'==' #'~~' value:value:
	   negated new new: value:value:value: value:value:value:value:
	   valueWithArguments: cull:cull: cull:cull:cull: cull:cull:cull:cull:
	   self name #< #'<=' #'>=' #> class Class Behavior ClassDescription
	   Metaclass SmallInteger noFallback ifTrue: ifTrue:ifFalse
	   ifFalse: ifFalse:ifTrue: ifNil: ifNil:ifNotNil ifNotNil: ifNotNil:ifNil:
	   perform: perform:with: perform:with:with: perform:with:with:with:
	   perform:withArguments: perform:withArguments:inSuperclass:
	   fibonacci Object ) do: [ :aString | self objectSymbol: aString ].
	extraSymbols removeLast: extraSymbols size.
	^ symbols
]

{ #category : 'writing' }
ASImageExporter >> writeClassTable [

	self halt
]

{ #category : 'writing' }
ASImageExporter >> writeImageSending: aSymbol to: anObject [

	| folder |
	folder := self imageFolder.
	(self imageSending: aSymbol to: anObject) toDirectory: folder.
	subHeaps do: [ : subHeap | subHeap toDirectory: folder]

]
