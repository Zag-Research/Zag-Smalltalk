"
I encode values the Zag way, which is an extension of SPUR
"
Class {
	#name : 'ZagImageEncodeZag',
	#superclass : 'ZagImageEncoder',
	#category : 'Zag-Exporter-Encoding',
	#package : 'Zag-Exporter',
	#tag : 'Encoding'
}

{ #category : 'converting' }
ZagImageEncodeZag class >> boolean: value [

	^ self literal: 0 class: ClassFalse + value asBit
]

{ #category : 'converting' }
ZagImageEncodeZag class >> double: aFloat for: anImage [

	| fullFloat tag |
	fullFloat := ((aFloat at: 1) bitShift: 32) + (aFloat at: 2).
	tag := fullFloat >> 60 + 2.
	tag & 6 = 0 ifTrue: [ ^ anImage objectHeapFloat: fullFloat ].
	^ fullFloat & 16rfffffffffffffff << 4 + tag
]

{ #category : 'class initialization' }
ZagImageEncodeZag class >> initialize [

	self initializeClassConstants.
	self initializeThreadedConstants.
	self initializeHeapConstants
]

{ #category : 'converting' }
ZagImageEncodeZag class >> literal: l class: c [

	^ (l bitShift: 8) + (c bitShift: 3) + 1
]

{ #category : 'magic' }
ZagImageEncodeZag class >> magic [

	^ 16r615A616D4967615A " ZagImaZa"
]

{ #category : 'converting' }
ZagImageEncodeZag class >> pointer: p [

	^ p
]

{ #category : 'converting' }
ZagImageEncodeZag class >> smallInteger: anInteger [

	^ self
		  literal: (anInteger bitAnd: 16r00ffffffffffffff)
		  class: ClassSmallInteger
]

{ #category : 'converting' }
ZagImageEncodeZag class >> symbolArity: arity hash: hash [ 
	^ self literal: (arity bitShift: 24) + hash class: ClassSymbol
]

{ #category : 'converting' }
ZagImageEncodeZag class >> undefinedObject [

	^ 0
]

{ #category : 'zag runtime alignment' }
ZagImageEncodeZag class >> zagClasses [

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

{ #category : 'zag runtime alignment' }
ZagImageEncodeZag class >> zagThreadesFns [
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
