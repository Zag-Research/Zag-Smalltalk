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
		'ClassDoubleWordArray',
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
		'T_ClassCase',
		'T_ClassCase24',
		'T_CullColon',
		'T_Drop',
		'T_DropNext',
		'T_Dup',
		'T_Label',
		'T_MakeImmediateClosure',
		'T_Over',
		'T_PopAssociationValue',
		'T_PopIndirect',
		'T_PopIndirectLocal',
		'T_PopInstVar',
		'T_PopLocal',
		'T_PopLocalData',
		'T_PopLocalField',
		'T_Primitive',
		'T_PrimitiveError',
		'T_PrimitiveModule',
		'T_PrimitiveModuleError',
		'T_PushAssociationValue',
		'T_PushContext',
		'T_PushIndirect',
		'T_PushIndirectLocal',
		'T_PushInstVar',
		'T_PushLiteral',
		'T_PushLocal',
		'T_PushLocalData',
		'T_PushLocalField',
		'T_PushStack',
		'T_PushThisContext',
		'T_PushThisProcess',
		'T_ReturnNonLocal',
		'T_ReturnSelf',
		'T_ReturnSelfNoContext',
		'T_ReturnTop',
		'T_ReturnTopNoContext',
		'T_Send',
		'T_StoreLocal',
		'T_Swap',
		'T_TailCallMethod',
		'T_TailSend',
		'T_TailSendNoContext',
		'T_Value',
		'T_ValueColon'
	],
	#category : 'Zag-Exporter-Encoding',
	#package : 'Zag-Exporter',
	#tag : 'Encoding'
}

{ #category : 'class initialization' }
ZagConstants class >> api [

	^ ZagImageOutputAPI
]

{ #category : 'converting' }
ZagConstants class >> asArray [
" - do not edit - instead `do` the following comment "
" self generateInitializeThreadedConstants "
	^ {
		T_Branch.
		T_Call.
		T_ClassCase.
		T_ClassCase24.
		T_CullColon.
		T_Drop.
		T_DropNext.
		T_Dup.
		T_Label.
		T_MakeImmediateClosure.
		T_Over.
		T_PopAssociationValue.
		T_PopIndirect.
		T_PopIndirectLocal.
		T_PopInstVar.
		T_PopLocal.
		T_PopLocalData.
		T_PopLocalField.
		T_Primitive.
		T_PrimitiveError.
		T_PrimitiveModule.
		T_PrimitiveModuleError.
		T_PushAssociationValue.
		T_PushContext.
		T_PushIndirect.
		T_PushIndirectLocal.
		T_PushInstVar.
		T_PushLiteral.
		T_PushLocal.
		T_PushLocalData.
		T_PushLocalField.
		T_PushStack.
		T_PushThisContext.
		T_PushThisProcess.
		T_ReturnNonLocal.
		T_ReturnSelf.
		T_ReturnSelfNoContext.
		T_ReturnTop.
		T_ReturnTopNoContext.
		T_Send.
		T_StoreLocal.
		T_Swap.
		T_TailCallMethod.
		T_TailSend.
		T_TailSendNoContext.
		T_Value.
		T_ValueColon.
	}

]

{ #category : 'converting' }
ZagConstants class >> asDoubleWordArray [

	^ DoubleWordArray withAll: self asArray
]

{ #category : 'class initialization' }
ZagConstants class >> generateInitializeThreadedConstants [

	<script>
	| itc adwa ltc |
	self class compile: 'asArray'.
	self class compile: 'initializeThreadedConstants'.
	self class compile: 'loadThreadedConstants: array'.
	self classVariables do: [ :variable |
		variable isReferenced ifFalse: [ self removeClassVariable: variable ] ].
	itc := WriteStream on: ''.
	itc
		<< 'initializeThreadedConstants';
		cr.
	itc
		<< $";
		<< ' - do not edit - instead `do` the following comment ';
		<< $";
		cr.
	itc
		<< $";
		<< ' self generateInitializeThreadedConstants ';
		<< $";
		cr.
	adwa := WriteStream on: ''.
	adwa
		<< 'asArray';
		cr.
	adwa
		<< $";
		<< ' - do not edit - instead `do` the following comment ';
		<< $";
		cr.
	adwa
		<< $";
		<< ' self generateInitializeThreadedConstants ';
		<< $";
		cr.
	adwa
		tab;
		<< '^ {';
		cr.
	ltc := WriteStream on: ''.
	ltc
		<< 'loadThreadedConstants: array';
		cr.
	ltc
		<< $";
		<< ' - do not edit - instead `do` the following comment ';
		<< $";
		cr.
	ltc
		<< $";
		<< ' self generateInitializeThreadedConstants ';
		<< $";
		cr.
	self api methodDictionary keys sorted withIndexDo: [
		:key
		:index |
		| variableName |
		variableName := ('T_' , key first asUppercase asString
		                 , key allButFirst) asSymbol.
		self
			classVariableNamed: variableName
			ifAbsent: [ self addClassVarNamed: variableName ].
		itc tab.
		(itc << variableName)
			<< ' := ';
			<< (index * 8) asString;
			<< $.;
			cr.
		adwa
			tab;
			tab;
			<< variableName;
			<< $.;
			cr.
		ltc tab.
		(ltc << variableName)
			<< ' := array at: ';
			<< index asString;
			<< $.;
			cr ].
	adwa
		tab;
		<< '}';
		cr.
	self class compile: itc contents.
	self class compile: adwa contents.
	self class compile: ltc contents.
	self initializeThreadedConstants
]

{ #category : 'class initialization' }
ZagConstants class >> initialize [

	self initializeClassConstants.
	self initializeThreadedConstants.
	self initializeHeapConstants 
]

{ #category : 'class initialization' }
ZagConstants class >> initializeClassConstants [

	ClassSymbol := 14.
	ClassSmallInteger := 15.
	ClassFalse := 16.
	ClassTrue := 17.
	ClassCharacter := 18.
	ClassUndefinedObject := 32.
	ClassFloat := 33.
	ClassString := 40.
	ClassDoubleWordArray := 42
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
" - do not edit - instead `do` the following comment "
" self generateInitializeThreadedConstants "
	T_Branch := 8.
	T_Call := 16.
	T_ClassCase := 24.
	T_ClassCase24 := 32.
	T_CullColon := 40.
	T_Drop := 48.
	T_DropNext := 56.
	T_Dup := 64.
	T_Label := 72.
	T_MakeImmediateClosure := 80.
	T_Over := 88.
	T_PopAssociationValue := 96.
	T_PopIndirect := 104.
	T_PopIndirectLocal := 112.
	T_PopInstVar := 120.
	T_PopLocal := 128.
	T_PopLocalData := 136.
	T_PopLocalField := 144.
	T_Primitive := 152.
	T_PrimitiveError := 160.
	T_PrimitiveModule := 168.
	T_PrimitiveModuleError := 176.
	T_PushAssociationValue := 184.
	T_PushContext := 192.
	T_PushIndirect := 200.
	T_PushIndirectLocal := 208.
	T_PushInstVar := 216.
	T_PushLiteral := 224.
	T_PushLocal := 232.
	T_PushLocalData := 240.
	T_PushLocalField := 248.
	T_PushStack := 256.
	T_PushThisContext := 264.
	T_PushThisProcess := 272.
	T_ReturnNonLocal := 280.
	T_ReturnSelf := 288.
	T_ReturnSelfNoContext := 296.
	T_ReturnTop := 304.
	T_ReturnTopNoContext := 312.
	T_Send := 320.
	T_StoreLocal := 328.
	T_Swap := 336.
	T_TailCallMethod := 344.
	T_TailSend := 352.
	T_TailSendNoContext := 360.
	T_Value := 368.
	T_ValueColon := 376.

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
	T_ReturnNonLocal := array at: 35.
	T_ReturnSelf := array at: 36.
	T_ReturnSelfNoContext := array at: 37.
	T_ReturnTop := array at: 38.
	T_ReturnTopNoContext := array at: 39.
	T_Send := array at: 40.
	T_StoreLocal := array at: 41.
	T_Swap := array at: 42.
	T_TailCallMethod := array at: 43.
	T_TailSend := array at: 44.
	T_TailSendNoContext := array at: 45.
	T_Value := array at: 46.
	T_ValueColon := array at: 47.

]
