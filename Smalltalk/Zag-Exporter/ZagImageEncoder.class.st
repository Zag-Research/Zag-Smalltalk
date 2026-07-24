"
I am an abstract class to show all the methods encoders need.
"
Class {
	#name : 'ZagImageEncoder',
	#superclass : 'Object',
	#classVars : [
		'AgeStatic',
		'BaseAddress',
		'FormatDirectIndexed',
		'FormatExternal',
		'FormatExternalNonObject',
		'FormatExternalWeakWithPointers',
		'FormatIndexed',
		'FormatIndexedNonObject',
		'FormatIndexedWeakWithPointers',
		'FormatNotIndexable',
		'MaxHeaderLength'
	],
	#classInstVars : [
		'zagClassMap',
		'zagThreadedMap',
		'undefinedObject',
		'classBoolean',
		'classTrue',
		'classFalse',
		'classCharacter',
		'classClass',
		'classFloat',
		'classMethod',
		'classObject',
		'classSmallInteger',
		'classString',
		'classSymbol',
		'classUndefinedObject',
		'classCompiledMethod'
	],
	#category : 'Zag-Exporter-Encoding',
	#package : 'Zag-Exporter',
	#tag : 'Encoding'
}

{ #category : 'converting' }
ZagImageEncoder class >> boolean: value [

	^ self literal: 0 class: (value ifTrue: [classTrue] ifFalse: [classFalse])
]

{ #category : 'converting' }
ZagImageEncoder class >> character: aCharacter [

	^ self literal: aCharacter asInteger class: classCharacter
]

{ #category : 'accessing' }
ZagImageEncoder class >> codeAddresses [

	^ DoubleWordArray withAll: (self zagThreadesFns collectWithIndex: [ :ignored :index |
		  index * 8 ])
]

{ #category : 'converting' }
ZagImageEncoder class >> convertToZag: anObject [

	^ anObject convertToZag: self
]

{ #category : 'converting' }
ZagImageEncoder class >> double: aNumber for: anImage [

	^ self subclassResponsibility
]

{ #category : 'class initialization' }
ZagImageEncoder class >> initializeAllConstants [

	self initializeClassConstants.
	self initializeThreadedConstants.
	self initializeHeapConstants
]

{ #category : 'class initialization' }
ZagImageEncoder class >> initializeClassConstants [


	zagClassMap := Dictionary newFromPairs: self zagClasses.
	classBoolean := zagClassMap at: #Boolean.
	classTrue := zagClassMap at: #True.
	classFalse := zagClassMap at: #False.
	classCharacter := zagClassMap at: #Character.
	classClass := zagClassMap at: #Class.
	classFloat := zagClassMap at: #Float.
	classMethod := zagClassMap at: #Method.
	classCompiledMethod := zagClassMap at: #CompiledMethod.
	classObject := zagClassMap at: #Object.
	classSmallInteger := zagClassMap at: #SmallInteger.
	classString := zagClassMap at: #String.
	classSymbol := zagClassMap at: #Symbol.
	classUndefinedObject := zagClassMap at: #UndefinedObject
]

{ #category : 'class initialization' }
ZagImageEncoder class >> initializeHeapConstants [

	AgeStatic := 7.
	BaseAddress := 16r100000.
	MaxHeaderLength := 4092.
	FormatNotIndexable := 112.
	FormatIndexedNonObject := 113.
	FormatExternalNonObject := 114.
	FormatDirectIndexed := 115.
	FormatIndexed := 116.
	FormatExternal := 117.
	FormatExternalWeakWithPointers := 126.
	FormatIndexedWeakWithPointers := 127
]

{ #category : 'class initialization' }
ZagImageEncoder class >> initializeThreadedConstants [

	zagThreadedMap := Dictionary new.
	self zagThreadesFns doWithIndex: [ :symbol :index |
		zagThreadedMap at: symbol put: index * 8 ]
]

{ #category : 'converting' }
ZagImageEncoder class >> literal: l class: c [

	^ self subclassResponsibility
]

{ #category : 'magic' }
ZagImageEncoder class >> magic [

	^ self subclassResponsibility 
]

{ #category : 'converting' }
ZagImageEncoder class >> pointer: p [

	^ p
]

{ #category : 'converting' }
ZagImageEncoder class >> smallInteger: anInteger [

	^ self subclassResponsibility
]

{ #category : 'converting' }
ZagImageEncoder class >> symbolArity: arity hash: hash [ 

	^ self subclassResponsibility
]

{ #category : 'converting' }
ZagImageEncoder class >> undefinedObject [

	^ self subclassResponsibility
]

{ #category : 'class initialization' }
ZagImageEncoder class >> updateClassVariables: mapping withCriteria: criteria [
	" remove unused class variables and add new ones "

	" remove current class variables that are no longer used "
	(self classVarNames select: [ :each |
		 (criteria value: each) and: [ (mapping includesKey: each) not ] ])
		do: [ :each | self removeClassVarNamed: each ].
	mapping keysDo: [ :each |
			| binding |
			"Check if the class variable already exists to avoid duplicates"
			(self hasClassVarNamed: each) ifFalse: [
				self addClassVarNamed: each ].
			binding := self classPool
				           associationAt: each
				           ifAbsent: [
				           self error:
					           'Class variable not found: ' , each asString ].
			binding value: (mapping at: each) ]
]

{ #category : 'zag runtime alignment' }
ZagImageEncoder class >> zagClasses [

	^ #(
	#ThunkReturnLocal 1
	#ThunkReturnInstance 2
	#ThunkReturnObject 3
	#ThunkReturnImmediate 4
	#ThunkLocal 5
	#BlockAssignLocal 6
	#ThunkInstance 7
	#BlockAssignInstance 8
	#ThunkHeap 9
	#ThunkImmediate 10
	#SmallInteger 11
	#False 12
	#True 13
	#Symbol 14
	#Character 15
	#Signature 16
	#ThunkReturnCharacter 17
	#ThunkReturnFloat 18
	#ThunkFloat 19
	#LLVM 20
	#UndefinedObject 21
	#Float 22
	#String 23
	#Utf8String 24
	#Context 32
	#ProtoObject 33
	#Object 34
	#Boolean 35
	#Array 36
	#ByteArray 37
	#DoubleWordArray 38
	#BlockClosure 39
	#Process 40
	#Class 41
	#CompiledMethod 42
	#Method 43
	#Dispatch 44
	#Association 45
	#Exception 46
	#Error 47
	#ContextData 48
	#SelectorException 49
	#PrimitiveFailed 50
	#BlockClosureValue 51
	#LLVMPrimitives 52
	#LLVMGenerator 53
	)
]

{ #category : 'zag runtime alignment' }
ZagImageEncoder class >> zagThreadesFns [
	^ #(
	#T_array
	#T_branch
	#T_branchFalse
	#T_branchTrue
	#T_classCase
	#T_countDown
	#T_createClosure
	#T_cullColon
	#T_debug
	#T_drop
	#T_dup
	#T_enddebug
	#T_fail
	#T_over
	#T_pop
	#T_popAssociationValue
	#T_primitive
	#T_primitiveError
	#T_primitiveModule
	#T_primitiveModuleError
	#T_push
	#T_pushAssociationValue
	#T_pushClosure
	#T_pushContext
	#T_pushLiteral
	#T_pushThisContext
	#T_pushThisProcess
	#T_returnLiteralClosure
	#T_returnLocalClosure
	#T_returnSelf
	#T_returnTop
	#T_returnTopNonLocal
	#T_send
	#T_send0
	#T_store
	#T_tailSend
	#T_value
	#T_valueColon
	#T_Float_add
	#T_Float_leq
	#T_Float_mul
	#T_Float_sub
	#T_SmallInteger_add
	#T_SmallInteger_leq
	#T_SmallInteger_mul
	#T_SmallInteger_sub
)
]
