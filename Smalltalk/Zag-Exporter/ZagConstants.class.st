"
I have all the constants to communicate with zag, so exported images can be loaded
"
Class {
	#name : 'ZagConstants',
	#superclass : 'SharedPool',
	#classVars : [
		'AgeStatic',
		'BaseAddress',
		'ClassArray',
		'ClassAssociation',
		'ClassBlockAssignInstance',
		'ClassBlockAssignLocal',
		'ClassBlockClosure',
		'ClassBlockClosureValue',
		'ClassCharacter',
		'ClassClass',
		'ClassCompiledMethod',
		'ClassContext',
		'ClassContextData',
		'ClassDispatch',
		'ClassDoubleWordArray',
		'ClassError',
		'ClassException',
		'ClassFalse',
		'ClassFloat',
		'ClassLLVM',
		'ClassLLVMGenerator',
		'ClassLLVMPrimitives',
		'ClassObject',
		'ClassPrimitiveFailed',
		'ClassProcess',
		'ClassProtoObject',
		'ClassSelectorException',
		'ClassSignature',
		'ClassSmallInteger',
		'ClassString',
		'ClassSymbol',
		'ClassThunkFloat',
		'ClassThunkHeap',
		'ClassThunkImmediate',
		'ClassThunkInstance',
		'ClassThunkLocal',
		'ClassThunkReturnCharacter',
		'ClassThunkReturnFloat',
		'ClassThunkReturnImmediate',
		'ClassThunkReturnInstance',
		'ClassThunkReturnLocal',
		'ClassThunkReturnObject',
		'ClassTrue',
		'ClassUndefinedObject',
		'ClassUtf8String',
		'FormatDirectIndexed',
		'FormatExternal',
		'FormatExternalNonObject',
		'FormatExternalWeakWithPointers',
		'FormatIndexed',
		'FormatIndexedNonObject',
		'FormatIndexedWeakWithPointers',
		'FormatNotIndexable',
		'MaxHeaderLength',
		'T_Float_add',
		'T_Float_leq',
		'T_Float_mul',
		'T_Float_sub',
		'T_SmallInteger_add',
		'T_SmallInteger_leq',
		'T_SmallInteger_mul',
		'T_SmallInteger_sub',
		'T_array',
		'T_branch',
		'T_branchFalse',
		'T_branchTrue',
		'T_classCase',
		'T_countDown',
		'T_createClosure',
		'T_cullColon',
		'T_debug',
		'T_drop',
		'T_dup',
		'T_enddebug',
		'T_fail',
		'T_over',
		'T_pop',
		'T_popAssociationValue',
		'T_primitive',
		'T_primitiveError',
		'T_primitiveModule',
		'T_primitiveModuleError',
		'T_push',
		'T_pushAssociationValue',
		'T_pushClosure',
		'T_pushContext',
		'T_pushLiteral',
		'T_pushThisContext',
		'T_pushThisProcess',
		'T_returnLiteralClosure',
		'T_returnLocalClosure',
		'T_returnSelf',
		'T_returnTop',
		'T_returnTopNonLocal',
		'T_send',
		'T_send0',
		'T_store',
		'T_tailSend',
		'T_value',
		'T_valueColon'
	],
	#category : 'Zag-Exporter-Encoding',
	#package : 'Zag-Exporter',
	#tag : 'Encoding'
}

{ #category : 'converting' }
ZagConstants class >> asArray [

	^ self zagThreadesFns collectWithIndex: [ : ignored : index | index * 8 ]
]

{ #category : 'converting' }
ZagConstants class >> asDoubleWordArray [

	^ DoubleWordArray withAll: self asArray
]

{ #category : 'class initialization' }
ZagConstants class >> initialize [

	self initializeClassConstants.
	self initializeThreadedConstants.
	self initializeHeapConstants 
]

{ #category : 'class initialization' }
ZagConstants class >> initializeClassConstants [

	self updateClassVariables: (Dictionary newFromPairs: self zagClasses) withCriteria: [ :each | each beginsWith: 'Class' ]
]

{ #category : 'class initialization' }
ZagConstants class >> initializeHeapConstants [

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
ZagConstants class >> initializeThreadedConstants [

	| dict |
	dict := Dictionary new.
	self zagThreadesFns doWithIndex: [ :symbol :index |
		dict at: symbol put: index * 8 ].
	self
		updateClassVariables: dict
		withCriteria: [ :each | each beginsWith: 'T_' ]
]

{ #category : 'class initialization' }
ZagConstants class >> loadThreadedConstants: array [
" - do not edit - instead `do` the following comment "
" self generateInitializeThreadedConstants "
	T_Branch := array at: 1.
	T_Call := array at: 2.
	T_ClassCase := array at: 3.
	T_ClassCase24 := array at: 4.
	T_CullColon := array at: 5.
	T_Drop := array at: 6.
	T_DropNext := array at: 7.
	T_Dup := array at: 8.
	T_Label := array at: 9.
	T_MakeImmediateClosure := array at: 10.
	T_Over := array at: 11.
	T_PopAssociationValue := array at: 12.
	T_PopIndirect := array at: 13.
	T_PopIndirectLocal := array at: 14.
	T_PopInstVar := array at: 15.
	T_PopLocal := array at: 16.
	T_PopLocalData := array at: 17.
	T_PopLocalField := array at: 18.
	T_Primitive := array at: 19.
	T_PrimitiveError := array at: 20.
	T_PrimitiveModule := array at: 21.
	T_PrimitiveModuleError := array at: 22.
	T_PushAssociationValue := array at: 23.
	T_PushContext := array at: 24.
	T_PushIndirect := array at: 25.
	T_PushIndirectLocal := array at: 26.
	T_PushInstVar := array at: 27.
	T_PushLiteral := array at: 28.
	T_PushLocal := array at: 29.
	T_PushLocalData := array at: 30.
	T_PushLocalField := array at: 31.
	T_PushStack := array at: 32.
	T_PushThisContext := array at: 33.
	T_PushThisProcess := array at: 34.
	T_ReturnSelf := array at: 35.
	T_ReturnSelfNoContext := array at: 36.
	T_ReturnTop := array at: 37.
	T_ReturnTopNoContext := array at: 38.
	T_ReturnTopNonLocal := array at: 39.
	T_ReturnTopNonLocalNoContext := array at: 40.
	T_Send := array at: 41.
	T_StoreLocal := array at: 42.
	T_Swap := array at: 43.
	T_TailCallMethod := array at: 44.
	T_TailSend := array at: 45.
	T_TailSendNoContext := array at: 46.
	T_Value := array at: 47.
	T_ValueColon := array at: 48.

]

{ #category : 'class initialization' }
ZagConstants class >> updateClassVariables: mapping withCriteria: criteria [
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

{ #category : 'class initialization' }
ZagConstants class >> zagClasses [

	^ #(
	#ClassThunkReturnLocal 1
	#ClassThunkReturnInstance 2
	#ClassThunkReturnObject 3
	#ClassThunkReturnImmediate 4
	#ClassThunkLocal 5
	#ClassBlockAssignLocal 6
	#ClassThunkInstance 7
	#ClassBlockAssignInstance 8
	#ClassThunkHeap 9
	#ClassThunkImmediate 10
	#ClassSmallInteger 11
	#ClassSymbol 12
	#ClassFalse 13
	#ClassTrue 14
	#ClassCharacter 15
	#ClassSignature 16
	#ClassThunkReturnCharacter 17
	#ClassThunkReturnFloat 18
	#ClassThunkFloat 19
	#ClassLLVM 20
	#ClassUndefinedObject 21
	#ClassFloat 22
	#ClassContext 32
	#ClassProtoObject 33
	#ClassObject 34
	#ClassArray 35
	#ClassString 36
	#ClassUtf8String 37
	#ClassDoubleWordArray 38
	#ClassBlockClosure 39
	#ClassProcess 40
	#ClassClass 41
	#ClassCompiledMethod 42
	#ClassDispatch 43
	#ClassAssociation 44
	#ClassException 45
	#ClassError 46
	#ClassContextData 47
	#ClassSelectorException 48
	#ClassPrimitiveFailed 49
	#ClassBlockClosureValue 50
	#ClassLLVMPrimitives 51
	#ClassLLVMGenerator 52
	)
]

{ #category : 'as yet unclassified' }
ZagConstants class >> zagThreadesFns [
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
