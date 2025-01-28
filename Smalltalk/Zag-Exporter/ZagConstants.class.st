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
		'T_PrimitiveModule',
		'T_PushAssociationValue',
		'T_PushContext',
		'T_PushIndirect',
		'T_PushIndirectLocal',
		'T_PushInstVar',
		'T_PushLiteral',
		'T_PushLocal',
		'T_PushLocalData',
		'T_PushLocalField',
		'T_PushThisContext',
		'T_ReturnNoContext',
		'T_ReturnNonLocal',
		'T_ReturnTop',
		'T_ReturnWithContext',
		'T_Send',
		'T_StoreLocal',
		'T_Swap',
		'T_TailCallMethod',
		'T_TailSend',
		'T_Value',
		'T_ValueColon'
	],
	#category : 'Zag-Exporter-Encoding',
	#package : 'Zag-Exporter',
	#tag : 'Encoding'
}

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
		T_PrimitiveModule.
		T_PushAssociationValue.
		T_PushContext.
		T_PushIndirect.
		T_PushIndirectLocal.
		T_PushInstVar.
		T_PushLiteral.
		T_PushLocal.
		T_PushLocalData.
		T_PushLocalField.
		T_PushThisContext.
		T_ReturnNoContext.
		T_ReturnNonLocal.
		T_ReturnTop.
		T_ReturnWithContext.
		T_Send.
		T_StoreLocal.
		T_Swap.
		T_TailCallMethod.
		T_TailSend.
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
	ZagImageOutputAPI methodDictionary keys sorted withIndexDo: [
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
	T_PrimitiveModule := 160.
	T_PushAssociationValue := 168.
	T_PushContext := 176.
	T_PushIndirect := 184.
	T_PushIndirectLocal := 192.
	T_PushInstVar := 200.
	T_PushLiteral := 208.
	T_PushLocal := 216.
	T_PushLocalData := 224.
	T_PushLocalField := 232.
	T_PushThisContext := 240.
	T_ReturnNoContext := 248.
	T_ReturnNonLocal := 256.
	T_ReturnTop := 264.
	T_ReturnWithContext := 272.
	T_Send := 280.
	T_StoreLocal := 288.
	T_Swap := 296.
	T_TailCallMethod := 304.
	T_TailSend := 312.
	T_Value := 320.
	T_ValueColon := 328.

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
	T_PrimitiveModule := array at: 20.
	T_PushAssociationValue := array at: 21.
	T_PushContext := array at: 22.
	T_PushIndirect := array at: 23.
	T_PushIndirectLocal := array at: 24.
	T_PushInstVar := array at: 25.
	T_PushLiteral := array at: 26.
	T_PushLocal := array at: 27.
	T_PushLocalData := array at: 28.
	T_PushLocalField := array at: 29.
	T_PushThisContext := array at: 30.
	T_ReturnNoContext := array at: 31.
	T_ReturnNonLocal := array at: 32.
	T_ReturnTop := array at: 33.
	T_ReturnWithContext := array at: 34.
	T_Send := array at: 35.
	T_StoreLocal := array at: 36.
	T_Swap := array at: 37.
	T_TailCallMethod := array at: 38.
	T_TailSend := array at: 39.
	T_Value := array at: 40.
	T_ValueColon := array at: 41.

]
