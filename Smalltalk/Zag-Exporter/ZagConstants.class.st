"
I have all the constants to communicate with zag, so exported images can be loaded
"
Class {
	#name : 'ZagConstants',
	#superclass : 'SharedPool',
	#classVars : [
		'AgeStatic',
		'BaseAddress',
		'ClassCharacter',
		'ClassFalse',
		'ClassFloat',
		'ClassSmallInteger',
		'ClassString',
		'ClassSymbol',
		'ClassTrue',
		'ClassUndefinedObject',
		'FormatDirectIndexed',
		'FormatExternal',
		'FormatExternalNonObject',
		'FormatExternalWeakWithPointers',
		'FormatIndexed',
		'FormatIndexedNonObject',
		'FormatIndexedWeakWithPointers',
		'FormatNotIndexable',
		'MaxHeaderLength',
		'T_Branch',
		'T_Call',
		'T_CallRecursive',
		'T_Drop',
		'T_DropNext',
		'T_Dup',
		'T_DynamicDispatch',
		'T_Fallback',
		'T_IfFalse',
		'T_IfNil',
		'T_IfNotNil',
		'T_IfTrue',
		'T_Over',
		'T_PopLocal',
		'T_PopLocal0',
		'T_PopLocalData',
		'T_PopLocalField',
		'T_PrintStack',
		'T_PushContext',
		'T_PushLiteral',
		'T_PushLiteral0',
		'T_PushLiteral1',
		'T_PushLiteral2',
		'T_PushLiteralFalse',
		'T_PushLiteralIndirect',
		'T_PushLiteralNil',
		'T_PushLiteralTrue',
		'T_PushLiteral_1',
		'T_PushLocal',
		'T_PushLocal0',
		'T_PushLocalData',
		'T_PushLocalField',
		'T_PushThisContext',
		'T_ReplaceLiteral',
		'T_ReplaceLiteral0',
		'T_ReplaceLiteral1',
		'T_ReturnNoContext',
		'T_ReturnNonLocal',
		'T_ReturnTop',
		'T_ReturnWithContext',
		'T_SetupSend',
		'T_SetupTailSend',
		'T_SetupTailSend0',
		'T_StoreLocal',
		'T_Swap',
		'T_VerifyMethod'
	],
	#classInstVars : [
		'primitiveOffset',
		'maxPrimitive',
		'next'
	],
	#category : 'Zag-Exporter-Encoding',
	#package : 'Zag-Exporter',
	#tag : 'Encoding'
}

{ #category : 'converting' }
ZagConstants class >> asDoubleWordArray [

	^ DoubleWordArray withAll:
		  { #T_Branch. #T_Call. #T_CallRecursive. #T_Drop. #T_DropNext.
		  #T_Dup. #T_DynamicDispatch. #T_Fallback. #T_IfFalse. #T_IfNil.
		  #T_IfNotNil. #T_IfTrue. #T_Over. #T_PopLocal. #T_PopLocal0.
		  #T_PopLocalData. #T_PopLocalField. #T_PrintStack. #T_PushContext.
		  #T_PushLiteral. #T_PushLiteral0. #T_PushLiteral1. #T_PushLiteral2.
		  #T_PushLiteralFalse. #T_PushLiteralIndirect. #T_PushLiteralNil.
		  #T_PushLiteralTrue. #T_PushLiteral_1. #T_PushLocal. #T_PushLocal0.
		  #T_PushLocalData. #T_PushLocalField. #T_PushThisContext.
		  #T_ReplaceLiteral. #T_ReplaceLiteral0. #T_ReplaceLiteral1.
		  #T_ReturnNoContext. #T_ReturnNonLocal. #T_ReturnTop. #T_ReturnWithContext.
		  #T_SetupSend. #T_SetupTailSend. #T_SetupTailSend0.
		  #T_StoreLocal. #T_Swap. #T_VerifyMethod } , ((1 to: maxPrimitive) collect: [ : p | p + primitiveOffset ])
]

{ #category : 'class initialization' }
ZagConstants class >> initialize [

	self initializeClassConstants.
	self initializeThreadedConstants.
	self initializeHeapConstants 
]

{ #category : 'class initialization' }
ZagConstants class >> initializeClassConstants [

	ClassFalse := 12.
	ClassTrue := 13.
	ClassSmallInteger := 14.
	ClassSymbol := 15.
	ClassCharacter := 16.
	ClassUndefinedObject := 32.
	ClassFloat := 33.
	ClassString := 40
]

{ #category : 'class initialization' }
ZagConstants class >> initializeHeapConstants [

	AgeStatic := 7.
	BaseAddress := 16r100000.
	MaxHeaderLength := 4093.
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
	next := 0.
	T_Branch := self next.
	T_Call := self next.
	T_CallRecursive := self next.
	T_Drop := self next.
	T_DropNext := self next.
	T_Dup := self next.
	T_DynamicDispatch := self next.
	T_Fallback := self next.
	T_IfFalse := self next.
	T_IfNil := self next.
	T_IfNotNil := self next.
	T_IfTrue := self next.
	T_Over := self next.
	T_PopLocal := self next.
	T_PopLocal0 := self next.
	T_PopLocalData := self next.
	T_PopLocalField := self next.
	T_PrintStack := self next.
	T_PushContext := self next.
	T_PushLiteral := self next.
	T_PushLiteral0 := self next.
	T_PushLiteral1 := self next.
	T_PushLiteral2 := self next.
	T_PushLiteralFalse := self next.
	T_PushLiteralIndirect := self next.
	T_PushLiteralNil := self next.
	T_PushLiteralTrue := self next.
	T_PushLiteral_1 := self next.
	T_PushLocal := self next.
	T_PushLocal0 := self next.
	T_PushLocalData := self next.
	T_PushLocalField := self next.
	T_PushThisContext := self next.
	T_ReplaceLiteral := self next.
	T_ReplaceLiteral0 := self next.
	T_ReplaceLiteral1 := self next.
	T_ReturnNoContext := self next.
	T_ReturnNonLocal := self next.
	T_ReturnTop := self next.
	T_ReturnWithContext := self next.
	T_SetupSend := self next.
	T_SetupTailSend := self next.
	T_SetupTailSend0 := self next.
	T_StoreLocal := self next.
	T_Swap := self next.
	T_VerifyMethod := self next.
	primitiveOffset := next.
	maxPrimitive := 0
]

{ #category : 'class initialization' }
ZagConstants class >> next [

	^ next := next + 8
]

{ #category : 'writing' }
ZagConstants class >> primitive: anInteger [

	maxPrimitive < anInteger ifTrue: [ maxPrimitive := anInteger ].
	^ primitiveOffset + (anInteger * 8)
]
