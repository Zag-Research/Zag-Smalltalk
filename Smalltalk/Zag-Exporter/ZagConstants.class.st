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
	T_Primitive := 136.
	T_PrimitiveFailed := 144.
	T_PrimitiveModule := 152.
	T_PushAssociationValue := 160.
	T_PushContext := 168.
	T_PushIndirect := 176.
	T_PushIndirectLocal := 184.
	T_PushInstVar := 192.
	T_PushLiteral := 200.
	T_PushLocal := 208.
	T_PushLocalData := 216.
	T_PushLocalField := 224.
	T_PushThisContext := 232.
	T_ReturnNoContext := 240.
	T_ReturnNonLocal := 248.
	T_ReturnTop := 256.
	T_ReturnWithContext := 264.
	T_Send := 272.
	T_StoreLocal := 280.
	T_Swap := 288.
	T_TailCallMethod := 296.
	T_TailSend := 304.
	T_Value := 312.
	T_ValueColon := 320.

]
