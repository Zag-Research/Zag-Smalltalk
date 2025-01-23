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
		'T_PrimitiveFailed',
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
ZagConstants class >> asDoubleWordArray [
" - do not edit - instead `do` the following comment "
" self generateInitializeThreadedConstants "
	^ DoubleWordArray withAll: {
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
		T_PrimitiveFailed.
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

{ #category : 'class initialization' }
ZagConstants class >> generateInitializeThreadedConstants [

	<script>
	| next itc adwa |
	self class compile: 'asDoubleWordArray'.
	self class compile: 'initializeThreadedConstants'.
	self classVariables do: [ :variable |
		variable isReferenced ifFalse: [ self removeClassVariable: variable ] ].
	next := 8.
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
		<< 'asDoubleWordArray';
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
		<< '^ DoubleWordArray withAll: {';
		cr.
	ZagImageOutputAPI methodDictionary keys sorted do: [ :key |
		| variableName |
		variableName := ('T_' , key first asUppercase asString
		                 , key allButFirst) asSymbol.
		self
			classVariableNamed: variableName
			ifAbsent: [ self addClassVarNamed: variableName ].
		itc tab.
		(itc << variableName)
			<< ' := ';
			<< next asString;
			<< $.;
			cr.
		next := next + 8.
		adwa
			tab;
			tab;
			<< variableName;
			<< $.;
			cr ].
	adwa
		tab;
		<< '}';
		cr.
	self class compile: itc contents.
	self class compile: adwa contents.
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
	T_PrimitiveFailed := 160.
	T_PrimitiveModule := 168.
	T_PushAssociationValue := 176.
	T_PushContext := 184.
	T_PushIndirect := 192.
	T_PushIndirectLocal := 200.
	T_PushInstVar := 208.
	T_PushLiteral := 216.
	T_PushLocal := 224.
	T_PushLocalData := 232.
	T_PushLocalField := 240.
	T_PushThisContext := 248.
	T_ReturnNoContext := 256.
	T_ReturnNonLocal := 264.
	T_ReturnTop := 272.
	T_ReturnWithContext := 280.
	T_Send := 288.
	T_StoreLocal := 296.
	T_Swap := 304.
	T_TailCallMethod := 312.
	T_TailSend := 320.
	T_Value := 328.
	T_ValueColon := 336.

]
